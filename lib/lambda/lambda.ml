let service = "lambda"

let base_url ~region =
    Format.sprintf "%s.%s.amazonaws.com" service region

(*let invoke ~func_name ~(region: Region.t) =
    let region = Region.show region in
    let base_url = base_url ~region in

    ()*)
