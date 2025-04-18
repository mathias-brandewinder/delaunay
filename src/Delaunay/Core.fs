namespace Delaunay

module Geometry =

    type Point = { X: float; Y: float }

    type Triangle = {
        A: Point
        B: Point
        C: Point
        }

    type Circle = {
        Center: Point
        Radius: float
        }

    let distance (a: Point, b: Point) =
        sqrt (pown (a.X - b.X) 2 + pown (a.Y - b.Y) 2)

    [<RequireQualifiedAccess>]
    module Circle =

        let contains (pt: Point) (circle: Circle) =
            distance (pt, circle.Center) <= circle.Radius

module BowyerWatson =

    open Geometry

    let superTriangleOLD (points: seq<Point>): Triangle =

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

        let xSize = xMax - xMin
        let ySize = yMax - yMin

        let pointA = {
            X = xMin - 1.0 * xSize
            Y = yMin - 0.5 * ySize
            }

        let pointB = {
            X = xMin + 2.0 * xSize
            Y = yMin - 0.5 * ySize
            }

        let pointC = {
            X = xMin + 0.5 * xSize
            Y = yMin + 2.0 * ySize
            }

        {
            A = pointA
            B = pointB
            C = pointC
        }

    // https://en.wikipedia.org/wiki/Circumcircle#Cartesian_coordinates_2
    let circumCircle (triangle: Triangle) =

        let a, b, c = triangle.A, triangle.B, triangle.C

        let d = 2.0 * (a.X * (b.Y - c.Y) + b.X * (c.Y - a.Y) + c.X * (a.Y - b.Y))
        let x =
            (pown a.X 2 + pown a.Y 2) * (b.Y - c.Y)
            +
            (pown b.X 2 + pown b.Y 2) * (c.Y - a.Y)
            +
            (pown c.X 2 + pown c.Y 2) * (a.Y - b.Y)
        let y =
            (pown a.X 2 + pown a.Y 2) * (c.X - b.X)
            +
            (pown b.X 2 + pown b.Y 2) * (a.X - c.X)
            +
            (pown c.X 2 + pown c.Y 2) * (b.X - a.X)

        let center = { X = x / d; Y = y / d }
        let radius = distance (center, a)

        { Center = center; Radius = radius }

    let superTriangle (points: seq<Point>) =
        let points = points |> Array.ofSeq
        let len = points.Length
        let circles =
            seq {
                for p1 in 0 .. len - 3 do
                    for p2 in (p1 + 1) .. len - 2 do
                        for p3 in (p2 + 1) .. len - 1 ->
                            {
                                A = points.[p1]
                                B = points.[p2]
                                C = points.[p3]
                            }
                            |> circumCircle
                            |> fun c -> { c with Radius = c.Radius * 1.1 }
                }
        let xs =
            circles
            |> Seq.collect (fun circle ->
                seq {
                    circle.Center.X - circle.Radius
                    circle.Center.X + circle.Radius }
                )
            |> Array.ofSeq
        let ys =
            circles
            |> Seq.collect (fun circle ->
                seq {
                    circle.Center.Y - circle.Radius
                    circle.Center.Y + circle.Radius
                    }
                )
            |> Array.ofSeq

        let xMin = xs |> Array.min
        let xMax = xs |> Array.max

        let yMin = ys |> Array.min
        let yMax = ys |> Array.max

        let xSize = xMax - xMin
        let ySize = yMax - yMin

        let pointA = {
            X = xMin - 1.0 * xSize
            Y = yMin - 0.5 * ySize
            }

        let pointB = {
            X = xMin + 2.0 * xSize
            Y = yMin - 0.5 * ySize
            }

        let pointC = {
            X = xMin + 0.5 * xSize
            Y = yMin + 2.0 * ySize
            }

        {
            A = pointA
            B = pointB
            C = pointC
        }

    let addPoint (point: Point) (triangles: Triangle []) =
        let badTriangles, goodTriangles =
            triangles
            |> Array.partition (fun triangle ->
                triangle
                |> circumCircle
                |> Circle.contains point
                )
        let edge (pt1: Point, pt2: Point) =
            if pt1.X < pt2.X
            then pt1, pt2
            elif pt1.X > pt2.X
            then pt2, pt1
            else
                if pt1.Y < pt2.Y
                then pt1, pt2
                else pt2, pt1

        let uniqueEdges =
            badTriangles
            |> Array.collect (fun triangle ->
                [|
                    triangle.A, triangle.B
                    triangle.B, triangle.C
                    triangle.C, triangle.A
                |]
                )
            |> Array.map edge
            |> Array.countBy id
            |> Array.filter (fun (_, count) -> count = 1)
            |> Array.map fst

        let newTriangles =
            uniqueEdges
            |> Array.map (fun (a, b) ->
                { A = a; B = b; C = point }
                )

        Array.append goodTriangles newTriangles

    let delaunay (points: seq<Point>) =
        let corners (triangle: Triangle) =
            set [ triangle.A; triangle.B; triangle.C ]
        let initial = superTriangle points
        let initialCorners = corners initial
        ([| initial |], points)
        ||> Seq.fold (fun triangulation point ->
            addPoint point triangulation
            )
        |> Array.filter (fun triangle ->
            Set.intersect (corners triangle) initialCorners
            |> Set.isEmpty
            )

module Plot =

    open Geometry

    let setAttribute name value = $"{name}=\"{value}\""

    module Stroke =
        type Attr =
            | Color of string
            | Width of float
            | Dashes of seq<int>
        let color value = setAttribute "stroke" value
        let width (value:float) = setAttribute "stroke-width" value
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
        | Point of (Point * float)
        | Polygon of list<Point>
        | Label of (Point * string)
        | Circle of (Point * float)

    type Dimension = {
        Width: float
        Height: float
        }

    type Box = {
        TopLeft: Point
        BottomRight: Point
        }
        with
        member this.Width =
            this.BottomRight.X - this.TopLeft.X
        member this.Height =
            this.BottomRight.Y - this.TopLeft.Y

    let merge (box1: Box) (box2: Box) =
        {
            TopLeft = {
                X = min box1.TopLeft.X box2.TopLeft.X
                Y = min box1.TopLeft.Y box2.TopLeft.Y
                }
            BottomRight = {
                X = max box1.BottomRight.X box2.BottomRight.X
                Y = max box1.BottomRight.Y box2.BottomRight.Y
                }
        }

    let box (shape: Shape) =
        match shape with
        | Point (point, size) ->
            {
                TopLeft = {
                    X = point.X - size
                    Y = point.Y - size
                    }
                BottomRight = {
                    X = point.X + size
                    Y = point.Y + size
                    }
            }
        | Circle (center, radius) ->
            {
                TopLeft = {
                    X = center.X - radius
                    Y = center.Y - radius
                    }
                BottomRight = {
                    X = center.X + radius
                    Y = center.Y + radius
                    }
            }
        | Polygon points ->
            let xs = points |> List.map (fun pt -> pt.X)
            let ys = points |> List.map (fun pt -> pt.Y)
            {
                TopLeft = {
                    X = xs |> List.min
                    Y = ys |> List.min
                    }
                BottomRight = {
                    X = xs |> List.max
                    Y = ys |> List.max
                    }
            }
        | Label (point, text) ->
            {
                TopLeft = {
                    X = point.X
                    Y = point.Y
                    }
                BottomRight = {
                    X = point.X + float text.Length * 10.0
                    Y = point.Y + 10.0
                    }
            }

    let viewbox (shapes: seq<Shape>) =

        let fullBox =
            shapes
            |> Seq.map box
            |> Seq.reduce merge

        let xMin = fullBox.TopLeft.X
        let yMin = fullBox.TopLeft.Y

        let xSize = fullBox.Width
        let ySize = fullBox.Height

        let margin = 0.05

        (xMin - margin * xSize, yMin - margin * ySize, xSize * (1.0 + (2. * margin)), ySize * (1.0 + (2. * margin)))

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
            $"""<circle cx="{point.X}" cy="{point.Y}" r="{radius}" {style}/>"""
        | Circle (point, radius) ->
            $"""<circle cx="{point.X}" cy="{point.Y}" r="{radius}" {style}/>"""
        | Polygon points ->
            points
            |> Seq.map (fun pt -> $"{pt.X},{pt.Y}")
            |> String.concat " "
            |> fun points ->
                $"""<polygon points="{points}" {style}/>"""
        | Label (pt, text) ->
            $"<text x=\"{pt.X}\" y=\"{pt.Y}\">{text}</text>"

    let polygon (style: list<Style>) (points: list<Point>) =
        Polygon points, style

    let point (style: list<Style>) (point: Point, radius: int) =
        Point(point, radius), style

    let circle (style: list<Style>) (point: Point, radius: float) =
        Circle(point, radius), style

    let label (point: Point, text: string) = Label(point, text), []

    let plot (width: float, height: float) (styledShapes: list<Shape * list<Style>>) =
        let xmin, ymin, xmax, ymax =
            styledShapes
            |> List.map fst
            |> viewbox
        let viewbox = $"viewbox=\"{xmin} {ymin} {xmax} {ymax}\""
        let contents =
            styledShapes
            |> List.map (fun (shape, style) ->
                let display = render style shape
                $"  {display}"
                )
            |> String.concat System.Environment.NewLine

        $"<svg width=\"{width}\" height=\"{height}\" {viewbox}>{System.Environment.NewLine}{contents}{System.Environment.NewLine}</svg>"