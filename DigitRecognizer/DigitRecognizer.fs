module DigitRecognizer
open System
open System.IO

type Image = { Label:int; Pixels:int[] }

let strToImage (str:string) = 
            str.Split(',')
            |> Array.map int
            |> (fun intArray -> { Label = intArray.[0]; Pixels = intArray.[1..] })

let linesToImages =  Seq.skip 1 >> Seq.map strToImage

let distance pixels1 pixels2 = 
                    Array.map2 (fun p1 p2 -> (p1 - p2)*(p1 - p2)) pixels1 pixels2
                    |> Array.sum

let classify =
    let trainingsampleData = 
        File.ReadLines(@"D:\MyData\Projects\ML\ML\trainingsample.csv")
        |> linesToImages
    
    let minDistance testImage = 
        trainingsampleData 
        |> Seq.minBy (fun image -> distance image.Pixels testImage.Pixels)

    minDistance



let computeAccuracy = 
    let validationsampleData = 
            File.ReadLines(@"D:\MyData\Projects\ML\ML\validationsample.csv")
            |> linesToImages
    
    let ok image = if (classify image).Label = image.Label then 1 else 0
                    
    let (total, okCount) = Seq.fold (fun (total, okCount) image -> (total + 1, okCount + (ok image) ) ) (0, 0) validationsampleData
    (fun () -> (float okCount)/(float total))


