module Battleship.TypeProvider

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System
open System.Reflection

/// Very shorthand, S = ship, H = hit, E = empty, M = miss
type State = S | H | E | M

let applyMove (grid:Map<_,_>) key = 
    match grid |> Map.find key  with
    | E -> M, grid |> Map.remove key |> Map.add key M
    | S -> H, grid |> Map.remove key |> Map.add key H
    | _ -> failwith "Fatal type provider error" // This will only get hit if we hit the same sequare twice

let hasWon (grid:Map<_,_>) =
    grid |> Seq.map (fun i -> i.Value) |> Seq.exists (fun i -> i = S) |> not

let createDocs grid target =
    match applyMove grid target with
    | H, grid ->
        match hasWon grid with
        | true -> "You win"
        | false -> "Hit!"
    | M, grid ->
        "Miss!"
    | _ -> failwith "Type provider error"

[<TypeProvider>]
type BattleshipProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Game.Battleships"
    let asm = Assembly.GetExecutingAssembly()
    let serviceType = ProvidedTypeDefinition(asm, ns, "ProvidedTypes", Some typeof<obj>, HideObjectMethods = true)
    //let g = new Game(10, [2;2;3])
    let possibleTargets =
        seq {
            for x in [0..9] do
                for y in [0..9] do
                    yield x, y
        }

    let grid = possibleTargets |> Seq.map (fun i -> i, E) |> Map.ofSeq

    let rec createProperty docs parent list (x, y) =
        let name = sprintf "%i,%i" x y
        let propertyType = ProvidedTypeDefinition(name, Some typeof<obj>)
        let newList = Seq.where (fun i -> i <> (x, y)) list
        let childDocs = createDocs grid (x, y)
        propertyType.AddMembersDelayed(fun () -> Seq.map (createProperty childDocs propertyType newList) newList |> List.ofSeq)
        let property = ProvidedProperty(name, propertyType, GetterCode = (fun _ -> <@@ new obj() @@>))
        property.AddXmlDoc(docs)
        parent.AddMember(propertyType)
        property

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "Battleship", Some typeof<obj>)
        myType.AddMember(ProvidedConstructor([], InvokeCode = fun _ -> <@@ new obj() @@>))
        let docs = "Stuff and things"
        myType.AddMembersDelayed(fun () -> Seq.map (createProperty docs serviceType possibleTargets) possibleTargets |> List.ofSeq)
        [myType]

    do
        this.AddNamespace(ns, serviceType :: createTypes())

[<assembly:TypeProviderAssembly>]
do ()