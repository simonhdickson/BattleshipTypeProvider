module ItWorksOnMyMachine.FSharpShip
open Microsoft.FSharp.Control

type Agent<'Msg> = MailboxProcessor<'Msg>

type Point =
    | Point of (int * int)

type Ship =
    | Ship of Point list

type ShootResult =
    | Miss
    | Hit
    | Sunk
    | Won

type Message =
    | Shoot of (int * int) * AsyncReplyChannel<ShootResult>
    | Test of (int * int) * AsyncReplyChannel<ShootResult>

type Game (shipList: Ship list) =
    let delete target (Ship points) =
        points |> List.filter (fun x -> x <> Point(target))
               |> Ship

    let deleteHit target =
        let isNotSunk (Ship points) = points <> []
        List.map (delete target) >> List.filter isNotSunk

    let onCollision target ships =
        let newShips = ships |> deleteHit target
        match newShips.Length with
        | 0 -> (Won, [])
        | n when n = ships.Length -> (Hit, newShips)
        | _ -> (Sunk, newShips)

    let fleet = Agent.Start(fun inbox ->
        let rec loop (ships: Ship list) =
            async {
                let! message = inbox.Receive()
                match message with
                | Shoot (target, replyChannel) ->
                    if Point(target) |> Game.anyHit ships then
                        let (output, newShips) = onCollision target ships
                        replyChannel.Reply output
                        return! loop newShips
                    else
                        replyChannel.Reply Miss
                        return! loop ships
                | Test (target, replyChannel) ->
                    match Point(target) |> Game.anyHit ships with
                    | true  -> replyChannel.Reply Hit
                    | false -> replyChannel.Reply Miss
                    return! loop ships
            }
        loop shipList )

    static member private anyHit ships point =
        let isShipHit (Ship points) = points |> Seq.exists (fun x -> x = point)
        ships |> List.exists isShipHit

    static member private generateShip (size, length) =
        let rnd = System.Random()
        let horizontal = rnd.Next 2 = 1
        let constant = rnd.Next size
        let vary = rnd.Next (size - length)
        let dots = [vary..vary + length - 1]
        let mapYFun = fun y -> Point(constant, y)
        let mapXFun = fun x -> Point(x, constant)
        if horizontal then
            dots |> List.map mapYFun |> Ship
        else
            dots |> List.map mapXFun |> Ship

    static member private generateLegalShip (size, length, existingShips) =
        let (Ship points) = Game.generateShip (size, length)
        if points |> List.exists (Game.anyHit existingShips) then
            Game.generateLegalShip (size, length, existingShips)
        else
            Ship(points)

    static member private generateShips size lengthList =
        let mapFun = fun (ships: Ship list) x -> (Game.generateLegalShip (size, x, ships)) :: ships
        lengthList |> List.fold mapFun []

    new () = Game ( [Ship([Point(1,2); Point(2,2)]); Ship([Point(4,5); Point(4,6)])] )

    new (size: int, lengthList: int list) =
        let ships = Game.generateShips size lengthList
        Game ships

    member this.Shoot target =
        fleet.PostAndReply (fun replyChannel -> Shoot (target, replyChannel))

    member this.Test target =
        fleet.PostAndReply (fun replyChannel -> Test (target, replyChannel))