[<EntryPoint>]
let main argv = 
    let max_iteration = 1000

    //x0 = scaled x coordinate of pixel (scaled to lie in the Mandelbrot X scale (-2.5, 1))
    let scaleX x =
        (float(x) - 25.0) / 10.0

    //y0 = scaled y coordinate of pixel (scaled to lie in the Mandelbrot Y scale (-1, 1))
    let scaleY y =
        (float(y) - 10.0) / 10.0

    let rec iterate x y x0 y0 iteration =
        if  x*x + y*y < 2.0*2.0  &&  iteration < max_iteration then
            let xtemp = x*x - y*y + x0
            let y' = 2.0*x*y + y0
            let x' = xtemp
            iterate x' y' x0 y0 (iteration + 1)
        else
            if iteration = max_iteration then
                " "
            else
                "@"

    let EvalulatePixel row column =
        let x0 = scaleX column
        let y0 = scaleY row     
        iterate 0.0 0.0 x0 y0 0

    let BuildRow row = 
        [0..35] 
            |> Seq.map (fun column -> EvalulatePixel row column)
            |> Seq.reduce (+) 

    //Build row by row
    [1..19] 
        |> Seq.map BuildRow 
        |> Seq.iter (fun row -> printfn "%s" row)

    0
