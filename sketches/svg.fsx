#load "../src/Delaunay/Core.fs"
open Delaunay
open Delaunay.Plot

open System

let points =
    let rng = Random 0
    List.init 20 (fun _ ->
        {
            X = rng.NextDouble() * 100. + 150.
            Y = rng.NextDouble() * 100. + 150.
        }
        )

let triangle = BowyerWatson.superTriangle points

[
    // render super triangle as a polygon
    [ triangle.A; triangle.B; triangle.C ]
    |> Polygon
    |> Plot.render
        [
            Style.Fill (Fill.Color "lightyellow")
            Style.Stroke (Stroke.Color "black")
            Style.Stroke (Stroke.Width 1)
            Style.Stroke (Stroke.Dashes [ 1; 1 ])
        ]
    // render each point
    for point in points do
        (point, 4)
        |> Point
        |> Plot.render
            [
                Style.Fill (Fill.Color "white")
                Style.Stroke (Stroke.Color "black")
            ]
]
|> String.concat System.Environment.NewLine
