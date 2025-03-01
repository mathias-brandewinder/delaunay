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
