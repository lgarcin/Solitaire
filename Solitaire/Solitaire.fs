// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Solitaire

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

module App =
    type Position =
        { x : int
          y : int }

    type Board = List<Position>

    type Peg = Position

    type Hole = Position

    type Model =
        { holes : Set<Hole>
          pegs : Set<Peg>
          selected : Option<Peg> }

    type Msg =
        | Select of Peg
        | Move of Hole

    type Range =
        {
            xmin : int;
            xmax : int;
            ymin : int;
            ymax : int
        }

    let board = [for i in -3..3 do for j in -3..3 do if abs(i)+abs(j)<=4 then yield {x=i;y=j}]

    let initModel =
        { pegs = Set.ofList board |> Set.remove {x=0;y=1}
          holes = Set.ofList [{x=0;y=1}]
          selected = None }

    let init() = initModel, Cmd.none

    let range =
        let xs = Set.map (fun (e)-> e.x) (Set.union initModel.holes initModel.pegs)
        let xmin = Set.minElement xs
        let xmax = Set.maxElement xs
        let ys = Set.map (fun (e)-> e.y) (Set.union initModel.holes initModel.pegs)
        let ymin = Set.minElement ys
        let ymax = Set.maxElement ys
        {
            xmin=xmin;
            xmax=xmax;
            ymin=ymin;
            ymax=ymax}

    let update msg model =
        match msg with
        | Select peg -> { model with selected = Some peg }, Cmd.none
        | Move hole ->
            match model.selected with
                | None -> model, Cmd.none
                | Some peg ->
                        let mid =
                            { x = (peg.x + hole.x) / 2
                              y = (peg.y + hole.y) / 2 }
                        let admissible =
                            ((peg.x = hole.x && abs(peg.y-hole.y)=2) || (peg.y = hole.y && abs(peg.x-hole.x)=2)) && model.pegs.Contains mid
                        if admissible then
                            { model with pegs =
                                             model.pegs
                                             |> Set.remove peg
                                             |> Set.add hole
                                             |> Set.remove mid
                                         holes =
                                             model.holes
                                             |> Set.remove hole
                                             |> Set.add peg
                                             |> Set.add mid
                                         selected = None }, Cmd.none
                        else model, Cmd.none

    let drawPeg peg selected dispatch =
        let color = match selected with
                        | None -> Color.Blue
                        | Some p -> if peg = p then Color.Green else Color.Blue
        View.Button(cornerRadius=10,
                    minimumWidthRequest=10.,
                    minimumHeightRequest=10.,
                    backgroundColor=color,
                    command = (fun () -> dispatch (Select peg))
                    ).GridRow(peg.x-range.xmin).GridColumn(peg.y-range.ymin)

    let drawHole hole dispatch =
        View.Button(cornerRadius=10,
                    minimumWidthRequest=10.,
                    minimumHeightRequest=10.,
                    borderColor=Color.Black,
                    borderWidth=2.,
                    backgroundColor=Color.Transparent,
                    command = (fun () -> dispatch (Move hole))
                    ).GridRow(hole.x-range.xmin).GridColumn(hole.y-range.ymin)

    let view (model : Model) dispatch =
        View.ContentPage
            (content = View.Grid
                           (verticalOptions = LayoutOptions.Center,
                           horizontalOptions=LayoutOptions.Center,
                            rowdefs = [ for i in 0..(range.xmax-range.xmin) -> box "*" ],
                            coldefs = [ for j in 0..(range.ymax-range.ymin) -> box "*" ],
                            children = [ for peg in model.pegs -> drawPeg peg model.selected dispatch]
                                       @ [ for hole in model.holes ->drawHole hole dispatch]))

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram init update view

type App() as app =
    inherit Application()

    let runner =
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif

        |> XamarinFormsProgram.run app
#if DEBUG

    // Uncomment this line to enable live update in debug mode.
    // See https://fsprojects.github.io/Fabulous/tools.html for further  instructions.
    //
    //do runner.EnableLiveUpdate()

#endif

// Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
// See https://fsprojects.github.io/Fabulous/models.html for further  instructions.

#if APPSAVE
let modelId = "model"

    override __.OnSleep() =
        let json =
            Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine
            ("OnSleep: saving model into app.Properties, json = {0}", json)
        app.Properties.[modelId] <- json

    override __.OnResume() =
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) ->
                Console.WriteLine
                    ("OnResume: restoring model from app.Properties, json = {0}",
                     json)
                let model =
                    Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>
                        (json)
                Console.WriteLine
                    ("OnResume: restoring model from app.Properties, model = {0}",
                     (sprintf "%0A" model))
                runner.SetCurrentModel(model, Cmd.none)
            | _ -> ()
        with ex ->
            App.program.onError
                ("Error while restoring model found in app.Properties", ex)

    override this.OnStart() =
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif
