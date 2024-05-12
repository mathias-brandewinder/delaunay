// https://en.wikipedia.org/wiki/Delaunay_triangulation
// https://en.wikipedia.org/wiki/Voronoi_diagram

open System
open System.IO

type Point = { X: float; Y: float }

// vertex = point / corner / junction
// method
// 1 triangle: circle center

let distance (a, b) =
    sqrt (pown (a.X - b.X) 2 + pown (a.Y - b.Y) 2)

type Circle = {
    Center: Point
    Radius: float
    }

// https://en.wikipedia.org/wiki/Circumcircle#Cartesian_coordinates_2
let circumCircle (a, b, c) =
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

let a = { X = -2.0; Y = 0.0 }
let b = { X = 0.0; Y = 0.0 }
let c = { X = -1.0; Y = 1.0 }

circumCircle (a, b, c)

let edge (pt1: Point, pt2: Point) =
    if pt1 <= pt2
    then (pt1, pt2)
    else (pt2, pt1)

type Triangle = {
    A: Point
    B: Point
    C: Point
    }
    with
    member this.Circle =
        circumCircle (this.A, this.B, this.C)
    member this.IsInside (pt: Point) =
        let circle = this.Circle
        distance (circle.Center, pt) < circle.Radius
    member this.Edges =
        [|
            (this.A, this.B)
            (this.B, this.C)
            (this.C, this.A)
        |]
        |> Array.map edge

// TODO fix this
let superTriangle (points: Point []) = {
    A = { X = 1000.0; Y = -500.0 }
    B = { X = -1000.0; Y = 500.0 }
    C = { X = 0; Y = 1000.0 }
    }

// https://en.wikipedia.org/wiki/Bowyer%E2%80%93Watson_algorithm#Pseudocode
let bowyerWatson (points: Point []) =
    let superTriangle = superTriangle points
    let superVertexes = set [ superTriangle.A; superTriangle.B; superTriangle.C ]
    let triangulation = Array.singleton superTriangle
    (triangulation, points)
    ||> Array.fold (fun tri point ->
        let badTriangles, goodTriangles =
            tri
            |> Array.partition (fun triangle -> triangle.IsInside point)
        let polygon =
            badTriangles
            |> Array.collect (fun t -> t.Edges)
            |> Array.countBy id
            |> Array.filter (fun (edge, count) -> count = 1)
            |> Array.map fst
        (goodTriangles, polygon)
        ||> Array.fold (fun triangulation (a, b) ->
            let triangle = { A = a; B = b; C = point }
            Array.append triangulation (Array.singleton triangle)
            )
        )
    |> Array.filter (fun triangle ->
        let vertexes = set [ triangle.A; triangle.B; triangle.C ]
        Set.intersect vertexes superVertexes
        |> Set.isEmpty
        )

module SVG =
    let private header = """
<!DOCTYPE html>
<html>
<head>
<title>SVG</title>
</head>
<body>
    <div width=200 height=200>
        <svg width=200 height=100% viewBox="0 0 200 200">
"""

    let private footer = """
        </svg>
    </div>
</body>
</html>
"""

    let circle (pt: Point) =
        $"""<ellipse cx="{pt.X}" cy="{pt.Y}" rx="3" ry="3"></ellipse>"""

    let line (pt1: Point, pt2: Point) =
        $"""<line x1="{pt1.X}" y1="{pt1.Y}" x2="{pt2.X}" y2="{pt2.Y}" style="stroke: Black;stroke-width:1" />"""

    let polygon (pts: seq<Point>) =
        pts
        |> Seq.map (fun pt -> $"{pt.X},{pt.Y}")
        |> String.concat " "
        |> sprintf """<polygon points="%s" stroke-dasharray="1,1" style="fill:lightyellow;stroke: Black;stroke-width:0.5" />"""

    let render (content: string) =
        sprintf $"""
{header}
{content}
{footer}
"""

let pts =
    let rng = Random 0
    Array.init 10 (fun _ ->
        { X = rng.NextDouble() * 100.0; Y = rng.NextDouble() * 100.0 }
        )

let delaunay =
    pts
    |> bowyerWatson

delaunay
|> Array.collect (fun triangle ->
    [|
        triangle.A |> SVG.circle
        triangle.B |> SVG.circle
        triangle.C |> SVG.circle
        yield!
            triangle.Edges
            |> Array.map SVG.line
    |]
    )
|> String.concat "\n"
|> SVG.render
|> fun svg ->
    File.WriteAllText(
        Path.Combine(__SOURCE_DIRECTORY__, "delaunay.html"),
        svg
        )