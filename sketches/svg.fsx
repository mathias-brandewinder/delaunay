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
    polygon
        [
            Style.Fill (Fill.Color "lightyellow")
            Style.Stroke (Stroke.Color "black")
            Style.Stroke (Stroke.Width 1)
            Style.Stroke (Stroke.Dashes [ 1; 1 ])
        ]
        [ triangle.A; triangle.B; triangle.C ]
    // render each point
    for pt in points do
        point
            [
                Style.Fill (Fill.Color "white")
                Style.Stroke (Stroke.Color "black")
            ]
            (pt, 4)
]
|> Plot.plot (400, 400)