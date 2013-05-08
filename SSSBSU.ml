open SS

let stage1 fh =
    (* we only need a single page here, so we'll make our own. *)
    let ss = new ss ~width:6 ~height:10 ~quadrant:FirstQuadrant
                    ~horizontal_axis_label:"p" ~vertical_axis_label:"q" ~fh in

    ss#header "Unitary groups";
    ss#render_description "Now we will compute the cohomology $H^* BSU$ by
    inductively analyzing related spaces.  We begin by computing the cohomology
    rings $H^* U(n)$, where our primary tool is the fibration \\[U(n-1) \\to
    U(n) \\to \\mathbb{C}^{2n} \\setminus \\{0\\} \\simeq S^{2n-1}.\\]  We
    identify $U(1) \\simeq S^1$, which has cohomology $H^* U(1) = \\Lambda[e_1]$
    for $|e_1| = 1$.  In general, we claim that $H^* U(n) = \\bigotimes_{i \\ge
    1} \\Lambda[e_{2i-1}]$.  Let's consider the case $n = 3$, for example, whose
    spectral sequence is illustrated at left.
    
    This spectral sequence collapses at this page, using an analysis in two
    parts.  Firstly, consider the indecomposable elements in the fiber column:
    they are all of odd degree, of dimension bounded by $2n-3$.  To support a
    differential, they must cross a large gap to reach the groups in the
    right-hand column, a distance of $2n-1$ across.  This means that
    differentials can occur only on the $E_{2n-1}$-page, of signature $d_{2n-1}:
    E_{2n-1}^{0, q} \\to E_{2n-1}^{2n-1, q - 2n}$.  The shift in vertical
    grading forces the differential to land below the $p$-axis, and so it cannot
    exist!

    Secondly, for any decomposable element $\\prod_{i \\in I} e_i$, we can apply
    the Leibniz rule to get \\[d\\left( \\prod_{i \\in I} e_I \\right) =
    \\sum_{i \\in I} \\pm d(e_i) \\prod_{\\substack{j \\in I \\\\ j \\ne i}}
    e_j.\\]  We just showed that $d(e_i) = 0$ for any $i$, and so the sum
    collapses, determining all those differentials to be zero as well.";

    ss#open_page ();
    ss#add_object ~name:"one" ~x:0 ~y:0 ~glyph:"\\Z" ~highlighted:false ~label:None;
    ss#add_object ~name:"e1" ~x:0 ~y:1 ~glyph:"\\Z" ~highlighted:false
        ~label:(Some {str="e_1"; direction = LeftOf; distance = None});
    ss#add_object ~name:"e3" ~x:0 ~y:3 ~glyph:"\\Z" ~highlighted:false
        ~label:(Some {str="e_3"; direction = LeftOf; distance = None});
    ss#add_object ~name:"e1e3" ~x:0 ~y:4 ~glyph:"\\Z" ~highlighted:false
        ~label:(Some {str="e_1e_3"; direction = LeftOf; distance = None});
    ss#add_object ~name:"e5" ~x:5 ~y:0 ~glyph:"\\Z" ~highlighted:false
        ~label:(Some {str="e_5"; direction = Below; distance = None});
    ss#add_object ~name:"e1e5" ~x:5 ~y:1 ~glyph:"\\Z" ~highlighted:false ~label:None;
    ss#add_object ~name:"e3e5" ~x:5 ~y:3 ~glyph:"\\Z" ~highlighted:false ~label:None;
    ss#add_object ~name:"e1e3e5" ~x:5 ~y:4 ~glyph:"\\Z" ~highlighted:false ~label:None;
    ss#close_page ();
    ()

let stage2 ss =
    ss#render_description "Next, we compute the cohomologies $H^* BU(n)$ using
    the fibration $U(n) \\to EU(n) \\to BU(n)$, where $EU(n) \\simeq
    \\pt$.  This is very similar to the computation for $\\CP^\\infty$,
    since the fiber sequence $S^1 \\to \\C^\\infty \\setminus \\{0\\} \\to
    \\CP^\\infty$ is equivalent to $U(1) \\to EU(1) \\to BU(1)$.  Since the
    total space is contractible, the goal in this game is to clear the board
    by introducing classes in $H^* BU(n)$ to delete the classes already
    present coming from $H^* U(n)$.
    
    At left, we consider the bottom of this spectral sequence for $n \\ge 4$. We
    have one chance to delete the class $e_1$, by introducing a class $x_1 \\in
    H^* BU(n)$ on page $E_2$, with differential $d(e_1) = x_1$.  Application of
    the Leibniz rule yields a whole host of resulting differentials.";
    ss#open_page ();
    ss#add_object ~name:"one" ~x:0 ~y:0 ~glyph:"\\Z" ~highlighted:false ~label:None;
    for i1 = 0 to 1 do
        for i3 = 0 to 1 do
            for i5 = 0 to 1 do
                for i7 = 0 to 1 do
                    for i9 = 0 to 1 do
                        let y = i1*1 + i3*3 + i5*5 + i7*7 + i9*9 in
                        if y <= 11 then begin
                            let name = if i1 > 0 then "e1" else "" in
                            let name = name ^ if i3 > 0 then "e3" else "" in
                            let name = name ^ if i5 > 0 then "e5" else "" in
                            let name = name ^ if i7 > 0 then "e7" else "" in
                            let name = name ^ if i9 > 0 then "e9" else "" in
                            let str = if i1 > 0 then "e_1" else "" in
                            let str = str ^ if i3 > 0 then "e_3" else "" in
                            let str = str ^ if i5 > 0 then "e_5" else "" in
                            let str = str ^ if i7 > 0 then "e_7" else "" in
                            let str = str ^ if i9 > 0 then "e_9" else "" in
                            if name <> "" then
                                ss#add_object ~name ~x:0 ~y ~glyph:"\\Z"
                                    ~highlighted:(i1 <> 0)
                                    ~label:(Some {str = str;
                                                  direction = LeftOf;
                                                  distance = None});
                            if i1 = 1 then begin
                                let target = if i3 > 0 then "e3" else "" in
                                let target = target ^ if i5 > 0 then "e5" else "" in
                                let target = target ^ if i7 > 0 then "e7" else "" in
                                let target = target ^ if i9 > 0 then "e9" else "" in
                                ss#add_arrow ~source:name
                                    ~target:(target^"x11")
                                    ~highlighted:true ~label:None
                            end;
                            for x = 1 to 5 do
                                let xname = name ^ "x1" ^ (string_of_int x) in
                                let label =
                                    if name = "" then
                                        let str = "x_1" ^
                                            if x = 1 then ""
                                                else "^"^(string_of_int x) in
                                        Some {str = str;
                                              direction = Below;
                                              distance = None}
                                    else None in
                                ss#add_object ~name:xname
                                    ~x:(x*2) ~y ~glyph:"\\Z"
                                    ~highlighted:(i1 <> 0 || x <> 0) ~label;
                                if i1 = 1 then
                                    let target = if i3 > 0 then "e3" else "" in
                                    let target = target ^ if i5 > 0 then "e5" else "" in
                                    let target = target ^ if i7 > 0 then "e7" else "" in
                                    let target = target ^ if i9 > 0 then "e9" else "" in
                                    if x <> 5 then
                                        ss#add_arrow ~source:xname
                                            ~target:(target^"x1"^(string_of_int(x+1)))
                                            ~highlighted:true ~label:None;
                            done
                        end
                    done
                done
            done
        done
    done;
    ss#close_page ();
    ()

let stage3 ss =
    ss#render_description "blah.";
    let object_names = ss#get_object_names () in
    let objects_to_keep = ["one"; "e3"; "e5"; "e7"; "e9"; "e3e5"] in
    let objects_to_delete = List.filter (fun name ->
        not (List.mem name objects_to_keep)) object_names in
    List.iter ss#delete_object objects_to_delete;
    ss#open_page ();
    for i3 = 0 to 1 do
        for i5 = 0 to 1 do
            for i7 = 0 to 1 do
                for i9 = 0 to 1 do
                    let y = i3*3 + i5*5 + i7*7 + i9*9 in
                    if y <= 9 then begin
                        let name = if i3 > 0 then "e3" else "" in
                        let name = name ^ if i5 > 0 then "e5" else "" in
                        let name = name ^ if i7 > 0 then "e7" else "" in
                        let name = name ^ if i9 > 0 then "e9" else "" in
                        let str = if i3 > 0 then "e_3" else "" in
                        let str = str ^ if i5 > 0 then "e_5" else "" in
                        let str = str ^ if i7 > 0 then "e_7" else "" in
                        let str = str ^ if i9 > 0 then "e_9" else "" in
                        if i3 <> 0 then
                            ss#highlight_object ~name true;
                        if i3 = 1 then begin
                            let target = if i5 > 0 then "e5" else "" in
                            let target = target ^ if i7 > 0 then "e7" else "" in
                            let target = target ^ if i9 > 0 then "e9" else "" in
                            ss#add_arrow ~source:name
                                ~target:(target^"x21")
                                ~highlighted:true ~label:None
                        end;
                        for x = 1 to 2 do
                            let xname = name ^ "x2" ^ (string_of_int x) in
                            let label =
                                if name = "" then
                                    let str = "x_2" ^
                                        if x = 1 then ""
                                            else "^"^(string_of_int x) in
                                    Some {str = str;
                                          direction = Below;
                                          distance = None}
                                else None in
                            ss#add_object ~name:xname
                                ~x:(x*4) ~y ~glyph:"\\Z"
                                ~highlighted:(i3 <> 0 || x <> 0) ~label;
                            if i3 = 1 then
                                let target = if i5 > 0 then "e5" else "" in
                                let target = target ^ if i7 > 0 then "e7" else "" in
                                let target = target ^ if i9 > 0 then "e9" else "" in
                                if x <> 2 then
                                    ss#add_arrow ~source:xname
                                        ~target:(target^"x2"^(string_of_int(x+1)))
                                        ~highlighted:true ~label:None;
                        done
                    end
                done
            done
        done
    done;
    ss#close_page ();
    ()


(*let stage3 ss =*)
    (*ss#open_page ();*)
     (*add more objects... *)
    (*for i = 2 to 4 do*)
        (*let xname = "x" ^ (string_of_int i) in*)
        (*let xlabel = "x^" ^ (string_of_int i) in*)
        (*let ename = "ex" ^ (string_of_int (i-1)) in*)
        (*ss#add_object ~name:xname ~x:(2*i) ~y:0 ~glyph:"\\mathbb{Z}"*)
            (*~highlighted:true ~label:*)
            (*(Some {str = xlabel; direction=Below; distance=None});*)
        (*ss#add_object ~name:ename ~x:(2*i-2) ~y:1 ~glyph:"\\mathbb{Z}"*)
            (*~highlighted:true ~label:None;*)
        (*ss#add_arrow ~source:ename ~target:xname ~highlighted:true ~label:None*)
    (*done;*)
    (*ss#add_object ~name:"ex4" ~x:8 ~y:1 ~glyph:"\\mathbb{Z}"*)
        (*~highlighted:true ~label:None;*)
    (*ss#close_page ();*)
    (*ss#render_description "But, if $E_2^{2, 0} =*)
        (*H^2(\\mathbb{C}\\mathrm{P}^\\infty; H^0(S^1; \\mathbb{Z}))$ is nonzero,*)
        (*then $E_2^{2, 1} = H^2(\\mathbb{C}\\mathrm{P}^\\infty; H^1(S^1;*)
        (*\\mathbb{Z}))$ is also nonzero, since $H^0(S^1; \\mathbb{Z}) \\cong*)
        (*H^1(S^1; \\mathbb{Z}) \\cong \\mathbb{Z}$.  The Serre spectral sequence*)
        (*is multiplicative, and so we already have a name for this element: $e*)
        (*\\cdot x$.  Moreover, $d_2$ is a derivation, so \\begin{align*} d_2(e*)
        (*\\cdot x) & = d_2(e) \\cdot x + (-1) e \\cdot d_2(x) \\ & = x^2 + 0 =*)
        (*x^2. \\end{align*}  For degree reasons, $e \\cdot x$ must also be killed*)
        (*on the $E_2$-page, and hence $x^2$ must exist in $E_2^{4, 0}$.  This*)
        (*pattern continues, as $d_2(e \\cdot x^n) = x^{n+1} + (-1) e \\cdot n*)
        (*x^{n-1} \\cdot 0 = x^{n+1}$."*)

(*let stage4 ss =*)
     (*deletion has to happen before we prerender the page! *)
    (*ss#delete_object "x";*)
    (*ss#delete_object "e";*)
    (*ss#delete_arrow "e" "x";*)
    (*for i = 2 to 4 do*)
        (*let xname = "x" ^ (string_of_int i) in*)
        (*let xlabel = "x^" ^ (string_of_int i) in*)
        (*let ename = "ex" ^ (string_of_int (i-1)) in*)
        (*ss#delete_object xname;*)
        (*ss#delete_object ename;*)
        (*ss#delete_arrow ename xname*)
    (*done;*)
    (*ss#delete_object "ex4";*)
    (*ss#open_page ();*)
    (*ss#close_page ();*)
    (*ss#render_description "To build the $E_3$ page, we take cohomology with the*)
    (*$d_2$ differentials, and we find nothing left but $1$ in the spectral*)
    (*sequence.  Hence, $E_3 \\cong E_\\infty$, and the spectral sequence*)
    (*collapses at $E_3$.*)

    (*Recall that $E_2^{p, 0} = H^p(\\mathbb{C}\\mathrm{P}^\\infty; H^0(S^1;*)
    (*\\mathbb{Z})) = H^p(\\mathbb{C}\\mathrm{P}^\\infty; \\mathbb{Z})$.  So, we*)
    (*can now read off the cohomology of $\\mathbb{C}\\mathrm{P}^\\infty$,*)
    (*together with its ring structure: \\[H^*(\\mathbb{C}\\mathrm{P}^\\infty;*)
    (*\\mathbb{Z}) \\cong \\mathbb{Z}[x],\\] where $|x| = 2$."*)

let output_bsu fh =
    (* initialize the spectral sequence *)
    let ss = new ss ~width:10 ~height:9 ~quadrant:FirstQuadrant
                    ~horizontal_axis_label:"p" ~vertical_axis_label:"q" ~fh in

    (* run through its stages *)
    stage1 fh;
    List.iter (fun x -> x ss) [stage2; stage3]
