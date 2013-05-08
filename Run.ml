let _ =
    let packages = ["cpinfty.tex", SSSCPinfty.output_cpinfty;
                    "bsu.tex", SSSBSU.output_bsu] in

    List.iter (fun (fname, routine) ->
        let fh = open_out fname in
        routine fh;
        close_out fh) packages;
