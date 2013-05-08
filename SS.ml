type term = Symbol (string * (int, int)) | (* (name, (pdeg, qdeg)) *)
            Prod (term list) | (* [mult1; ...; multn] *)
            Sum (term list) | (* [sum1; ...; sum1] *)
            Scale (int * term) | (* (n, expr) *)
            Zero (* 0, used for reducing *)

(* write term in DNF *)
let reduce_expr modulus expr =
    match expr with
    |Symbol _ -> expr
    |Prod a -> Prod a (* distribution, collection, propagate Zero *)
    |Sum a -> Sum a (* factorization of scalars, remove Zero instances *)
    |Scale (n, a) -> Scale (n, a) (* n % mod, replace Scale (0, _) with Zero *)
    |Zero -> Zero

(* apply leibniz rules to an expression *)
let differentiate rules expr =
    let differentiated = begin match expr with
    |Zero -> Zero
    |Symbol -> Zero (* loop up in rules table *)
    |Sum a -> Sum (List.map (differentiate rules) a)
    |Scale (n, a) -> Scale (n, differentiate rules a)
    |Prod a -> Sum Zero (* leibniz rule *)
    end;
    reduce_expr differentiated

type direction = Above | Below | LeftOf | RightOf
type label_t = {
    str : string;
    direction : direction;
    distance : float option;
}
type ss_obj = {
    name : string;
    x : float;
    y : float;
    glyph : string;
    mutable highlighted : bool;
    mutable label : label_t option;
}
type ss_arr = {
    source : string;
    target : string;
    mutable highlight : bool;
    mutable lbl : label_t option;
}

let grid_scale_factor = 1.0

let increment_position_slot arr x y =
    let step_size_x, step_size_y = 0.3, 0.3 in
    let (current_x, current_y) = arr.(x).(y) in
    if (float_of_int y -. step_size_y < current_y) then begin
        let loops = int_of_float ((current_x -. float_of_int x) /. step_size_x) in
        arr.(x).(y) <- (float_of_int x *. grid_scale_factor +. 0.25 *.
        (1.0 +. (-1.0) ** (float_of_int loops)) *. step_size_x,
            float_of_int y *. grid_scale_factor +.
                float_of_int (loops+1) *. step_size_y)
    end else begin
        arr.(x).(y) <- (current_x +. step_size_x, current_y -. step_size_y)
    end;
    (current_x, current_y)

type quadrant = FirstQuadrant | SecondQuadrant | ThirdQuadrant | FourthQuadrant |
                LeftHalf | RightHalf | TopHalf | BottomHalf | FullPlane
class ss ~width ~height ~quadrant ~horizontal_axis_label ~vertical_axis_label ~fh =
    object (self)
	    initializer
		    (* we draw spectral sequences on the left-hand page. *)
			output_string fh "\\advancetoleft\n"

        val mutable object_list = []
        val mutable arrow_list = []
        val mutable new_locations =
            Array.init (2*width+1) (fun p ->
                Array.init (2*height+1) (fun q ->
                    (float_of_int p *. grid_scale_factor,
                     float_of_int q *. grid_scale_factor)))

        method get_object_names () =
            List.map (fun x -> x.name) object_list

        method get_arrow_names () =
            List.map (fun x -> (x.source, x.target)) arrow_list

        method private render_obj obj =
            output_string fh "\\node[group";
            if obj.highlighted then
                output_string fh ",color=red";
            Printf.fprintf fh "] (%s) at (%f, %f) {$%s$};\n"
                obj.name obj.x obj.y obj.glyph;
            match obj.label with
            |Some {str = str; direction = direction; distance = Some distance} ->
                failwith "distance not yet implemented"
            |Some {str = str; direction = direction; distance = None} ->
                let direction = match direction with
                    |LeftOf -> "left"
                    |RightOf -> "right"
                    |Above -> "above"
                    |Below -> "below" in
                output_string fh "\\node[label";
                if obj.highlighted then
                    output_string fh ",color=red";
                Printf.fprintf fh
                    ",%s=of %s] {$%s$};\n" direction obj.name str;
            |None -> ()

        method private render_arrow arrow =
            output_string fh "\\draw[->";
            if arrow.highlight then
                output_string fh ",color=red";
            Printf.fprintf fh "] (%s) to (%s);\n" arrow.source arrow.target;
            ()

        method add_object ~name ~x ~y ~glyph ~highlighted ~label =
            let (x, y) = increment_position_slot new_locations x y in
            let new_object = {name        = name;
                              x           = x;
                              y           = y;
                              glyph       = glyph;
                              highlighted = highlighted;
                              label       = label} in
            object_list <- new_object :: object_list;
            self#render_obj new_object

        method header str =
            Printf.fprintf fh "\\section{%s}\n" str

        method add_label () = ()
        method remove_label () = ()
        method highlight_object ~name highlighted =
            List.iter (fun obj ->
                if obj.name = name then obj.highlighted <- highlighted)
            object_list

        method add_arrow ~source ~target ~highlighted ~label =
            let new_arrow = {source    = source;
                             target    = target;
                             highlight = highlighted;
                             lbl       = label} in
            arrow_list <- new_arrow :: arrow_list;
            self#render_arrow new_arrow;
            ()

        method delete_arrow source target =
            arrow_list <- List.filter (fun arr ->
                (arr.target <> target) && (arr.source <> source)) arrow_list

        method delete_object name =
            let arrows_to_delete = List.filter (fun arrow ->
                arrow.target = name || arrow.source = name) arrow_list in
            List.iter (fun arr ->
                self#delete_arrow (arr.source) (arr.target)) arrows_to_delete;
            object_list <- List.filter (fun obj -> obj.name <> name) object_list

        method open_page () =
            output_string fh "\\vspace*{\\fill}\n";
            let (left_bound, right_bound, top_bound, bottom_bound) =
                match quadrant with
                |FirstQuadrant -> (-1.0, float_of_int width +. 1.0,
                                   float_of_int height +. 1.0, -1.0)
                |_ -> failwith "quadrant not yet implemented" in
            (* open the picture *)
            output_string fh "\\begin{tikzpicture}[group/.style={},auto]\n";
            (* set up the bounding box *)
            Printf.fprintf fh
                "\\draw[use as bounding box,white] (%f, %f) rectangle (%f,%f);\n"
                left_bound bottom_bound right_bound top_bound;
            (* draw the grid *)
            Printf.fprintf fh
                "\\path [draw, gray!50, very thin] (%f, %f) grid (%f, %f);\n"
                (left_bound +. 1.0) (bottom_bound +. 1.0)
                (right_bound -. 1.0) (top_bound -. 1.0);

            (* draw extending arrows *)
            (* rightward arrow *)
            begin match quadrant with
            |FirstQuadrant ->
                Printf.fprintf fh
                    "\\draw[->,gray!50,thin] (0, 0) to (%f, 0);\n" right_bound;
            |_ -> failwith "quadrant not yet implemented" end;
            (* upward arrow *)
            begin match quadrant with
            |FirstQuadrant ->
                Printf.fprintf fh
                    "\\draw[->,gray!50,thin] (0, 0) to (0, %f);\n" top_bound;
            |_ -> failwith "quadrant not yet implemented" end;

            (* label axes *)
            for i = int_of_float left_bound + 1 to int_of_float right_bound - 1 do
                Printf.fprintf fh
                    "\\node[label,gray!50] at (%d, -0.5) {$%d$};\n" i i
            done;
            Printf.fprintf fh
                "\\node[label,gray!50] at (%f, -0.5) {$%s$};\n"
                right_bound horizontal_axis_label;
            for j = int_of_float bottom_bound + 1 to int_of_float top_bound - 1 do
                Printf.fprintf fh
                    "\\node[label,gray!50] at (-0.5, %d) {$%d$};\n" j j
            done;
            Printf.fprintf fh
                "\\node[label,gray!50] at (-0.5, %f) {$%s$};\n"
                top_bound vertical_axis_label;

            (* draw preexisting objects *)
            List.iter (fun obj -> obj.highlighted <- false) object_list;
            List.iter self#render_obj object_list;

            (* set up clipping for overflow arrows *)
            output_string fh "\\begin{scope}\n";
            Printf.fprintf fh
                "\\clip (%f, %f) rectangle (%f, %f);\n"
                (left_bound -. 1.0) (bottom_bound +. 1.0)
                (right_bound -. 1.0) (top_bound -. 1.0);

            (* draw preexisting arrows *)
            List.iter (fun arr -> arr.highlight <- false) arrow_list;
            List.iter self#render_arrow arrow_list;
            output_string fh "\\end{scope}\n";
            ()

            (* TODO: Clipping should be handled differently for objects,
             * arrows, and labels. This may require some reorganization. *)

        method close_page () =
            (* each spectral sequence page needs to be closed off in tikz. *)
            output_string fh "\\end{tikzpicture}\n";
            output_string fh "\\vspace*{\\fill}\n";
            output_string fh "\\newpage\n";
            ()

        method render_description str =
            output_string fh "\\vspace*{\\fill}\n";
            output_string fh str;
            output_string fh "\\vspace*{\\fill}\n";
            output_string fh "\\newpage\n";
    end
