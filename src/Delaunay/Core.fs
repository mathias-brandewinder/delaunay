namespace Delaunay

type Point = { X: float; Y: float }

type Triangle = {
    A: Point
    B: Point
    C: Point
    }

module BowyerWatson =

    let superTriangle (points: seq<Point>): Triangle =

        let xs =
            points 
            |> Seq.map (fun pt -> pt.X)
            |> Array.ofSeq
        let xMin = xs |> Array.min
        let xMax = xs |> Array.max

        let ys =
            points
            |> Seq.map (fun pt -> pt.Y)
            |> Array.ofSeq
        let yMin = ys |> Array.min
        let yMax = ys |> Array.max

        let squareWidth =
            max (xMax - xMin) (yMax - yMin)

        let pointA = {
            X = xMin - 0.5 * squareWidth
            Y = yMin
            }

        let pointB = {
            X = xMin + 1.5 * squareWidth
            Y = yMin
            }

        let pointC = {
            X = xMin + 0.5 * squareWidth
            Y = yMin + 2.0 * squareWidth
            }

        {
            A = pointA
            B = pointB
            C = pointC
        }

module Plot =

    let setAttribute name value = $"{name}=\"{value}\""

    module Stroke =
        type Attr =
            | Color of string
            | Width of int
            | Dashes of seq<int>
        let color value = setAttribute "stroke" value
        let width (value:int) = setAttribute "stroke-width" value
        let dashes pattern =
            pattern
            |> Seq.map string
            |> String.concat " "
            |> setAttribute "stroke-dasharray"
        let render attr =
            match attr with
            | Color value -> color value
            | Width value -> width value
            | Dashes value -> dashes value

    module Fill =
        type Attr =
            | Color of string
        let render attr =
            match attr with
            | Color value -> setAttribute "fill" value

    type Style =
        | Stroke of Stroke.Attr
        | Fill of Fill.Attr

    type Shape =
        | Point of (Point * int)
        | Polygon of list<Point>

    let render (style: seq<Style>) (shape: Shape) =
        let style =
            style
            |> Seq.map (fun s ->
                match s with
                | Stroke stroke -> Stroke.render stroke
                | Fill fill -> Fill.render fill
                )
            |> String.concat " "
        match shape with
        | Point (point, radius) ->
            $"""<ellipse cx="{point.X}" cy="{point.Y}" rx="{radius}" ry="{radius}" {style}/>"""
        | Polygon points ->
            points
            |> Seq.map (fun pt -> $"{pt.X},{pt.Y}")
            |> String.concat " "
            |> fun points ->
                $"""<polygon points="{points}" {style}/>"""
