module Mavnn.Blog.TypeProvider

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System
open System.Reflection
open ItWorksOnMyMachine.FSharpShip

[<TypeProvider>]
type BattleshipProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Game.Battleships"
    let asm = Assembly.GetExecutingAssembly()
    let serviceType = ProvidedTypeDefinition(asm, ns, "ProvidedTypes", Some typeof<obj>, HideObjectMethods = true)
    let g = new Game(10, [2;2;3])
    let possibleTargets =
        seq {
            for x in [0..9] do
                for y in [0..9] do
                    yield x, y
        }

    let createDocs target =
        match g.Test target with
        | Hit  -> "Hit!"    
        | Miss -> "Miss!"
        | Sunk -> "Sunk!"
        | Won -> "Won!"

    let rec createProperty parent list (x, y) =
        let name = sprintf "%i,%i" x y
        let propertyType = ProvidedTypeDefinition(name, Some typeof<obj>)
        let newList = Seq.where (fun i -> i <> (x, y)) list
        propertyType.AddMembersDelayed(fun () -> Seq.map (createProperty propertyType newList) newList |> List.ofSeq)
        let property = ProvidedProperty(name, propertyType, GetterCode = (fun args -> <@@ new obj() @@>))
        property.AddXmlDocDelayed(fun () -> createDocs (x, y))
        parent.AddMember(propertyType)
        property

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "Battleship", Some typeof<obj>)
        myType.AddMember(ProvidedConstructor([], InvokeCode = fun args -> <@@ new obj() @@>))
        myType.AddMembersDelayed(fun () -> Seq.map (createProperty serviceType possibleTargets) possibleTargets |> List.ofSeq)
        [myType]

    do
        this.AddNamespace(ns, serviceType :: createTypes())

[<assembly:TypeProviderAssembly>]
do ()