// include Fake lib
#r @"packages\FAKE\tools\FakeLib.dll"
#r @"packages\FSharpLint\FSharpLint.Framework.dll"
#r @"packages\FSharpLint\FSharpLint.Application.dll"
open Fake
open System
open FSharpLint.Application


let solutionFile  = "FSharp-PC-Axis"
let calculonProjectFile = "FSharp-PC-Axis/FSharp-PC-Axis.fsproj"
let binDirs = ["FSharp-PC-Axis/bin"]

Target "Clean" (fun _ ->
    CleanDirs binDirs
)

Target "Build" (fun _ -> 
    !! (solutionFile + ".sln") 
    |> MSBuildRelease "" "Rebuild" 
    |> ignore 
) 

Target "RestorePackages" DoNothing

"Clean" ==> "Build"

// start build
RunTargetOrDefault "Build"