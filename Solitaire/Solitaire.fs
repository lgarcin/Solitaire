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
        | Restart

    type Range =
        { xmin : int
          xmax : int
          ymin : int
          ymax : int }

    type GameStatus =
        | Win
        | Lose
        | Continue

    let board =
        [ for i in -3..3 do
              for j in -3..3 do
                  if abs (i) + abs (j) <= 4 then
                      yield { x = i
                              y = j } ]

    let initModel =
        { pegs =
              Set.ofList board
              |> Set.remove { x = 0
                              y = 1 }
          holes =
              Set.ofList [ { x = 0
                             y = 1 } ]
          selected = None }

    let gameStatus (model : Model) : GameStatus =
        let movable peg =
            (model.pegs.Contains { x = peg.x + 1
                                   y = peg.y }
             && model.holes.Contains { x = peg.x + 2
                                       y = peg.y })
            || (model.pegs.Contains { x = peg.x - 1
                                      y = peg.y }
                && model.holes.Contains { x = peg.x - 2
                                          y = peg.y })
            || (model.pegs.Contains { x = peg.x
                                      y = peg.y + 1 }
                && model.holes.Contains { x = peg.x
                                          y = peg.y + 2 })
            || (model.pegs.Contains { x = peg.x
                                      y = peg.y - 1 }
                && model.holes.Contains { x = peg.x
                                          y = peg.y - 2 })
        if (model.pegs.Count = 1) then Win
        elif List.contains true [ for p in model.pegs -> movable p ] then
            Continue
        else Lose

    let init() = initModel, Cmd.none

    let range =
        let xs =
            Set.map (fun e -> e.x) (Set.union initModel.holes initModel.pegs)
        let xmin = Set.minElement xs
        let xmax = Set.maxElement xs
        let ys =
            Set.map (fun e -> e.y) (Set.union initModel.holes initModel.pegs)
        let ymin = Set.minElement ys
        let ymax = Set.maxElement ys
        { xmin = xmin
          xmax = xmax
          ymin = ymin
          ymax = ymax }

    let update msg model =
        match msg with
        | Restart -> init()
        | Select peg -> { model with selected = Some peg }, Cmd.none
        | Move hole ->
            match model.selected with
            | None -> model, Cmd.none
            | Some peg ->
                let mid =
                    { x = (peg.x + hole.x) / 2
                      y = (peg.y + hole.y) / 2 }

                let admissible =
                    ((peg.x = hole.x && abs (peg.y - hole.y) = 2)
                     || (peg.y = hole.y && abs (peg.x - hole.x) = 2))
                    && model.pegs.Contains mid
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
        let color =
            match selected with
            | None -> Color.Blue
            | Some p ->
                if peg = p then Color.Green
                else Color.Blue
        View.Button(cornerRadius = 10 ,minimumWidth = 10.,
                    minimumHeight = 10., backgroundColor = color,
                    command = (fun () -> dispatch (Select peg)))
            .Row(peg.x - range.xmin).Column(peg.y - range.ymin)

    let drawHole hole dispatch =
        View.Button(cornerRadius = 10, minimumWidth = 10.,
                    minimumHeight = 10., borderColor = Color.Black,
                    borderWidth = 2., backgroundColor = Color.Transparent,
                    command = (fun () -> dispatch (Move hole)))
            .Row(hole.x - range.xmin).Column(hole.y - range.ymin)

    let view (model : Model) dispatch =
        View.ContentPage
            (content = match gameStatus model with
                       | Win ->
                           View.StackLayout
                               (children = [ View.Label(text = "Gagné")

                                             View.Button
                                                 (text = "Rejouer",
                                                  command = (fun () ->
                                                  dispatch Restart)) ])
                       | Lose ->
                           View.StackLayout
                               (children = [ View.Label(text = "Perdu")

                                             View.Button
                                                 (text = "Rejouer",
                                                  command = (fun () ->
                                                  dispatch Restart)) ])
                       | Continue ->
                           View.Grid
                               (verticalOptions = LayoutOptions.Center,
                                horizontalOptions = LayoutOptions.Center,
                                rowdefs = [ for i in 0..(range.xmax - range.xmin) ->
                                                Star ],
                                coldefs = [ for j in 0..(range.ymax - range.ymin) ->
                                                Star ],
                                children = [ for peg in model.pegs ->
                                                 drawPeg peg model.selected
                                                     dispatch ]
                                           @ [ for hole in model.holes ->
                                                   drawHole hole dispatch ]))

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
