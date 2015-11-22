module Helper

open System

let toDouble (str : string) = 
    (str.Replace ('.', ',')).Replace ("$", "")
    |> Convert.ToDouble 

let toDate (str : string) = 
    str
    |> Convert.ToDateTime