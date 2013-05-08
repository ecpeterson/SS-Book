open SS

let stage1 ss =
    ss#header "$H^* \\CP^\\infty$ redux";
    ss#render_description "Consider the spherical fibration \\[S^1 \\to \\mathbb{C}^\\infty \\setminus \\{0\\} \\to \\mathbb{C}\\mathrm{P}^\\infty.\\]  The total space, $\\mathbb{C}^\\infty \\setminus \\{0\\} \\simeq S^\\infty$, is contractible, hence has vanishing cohomology.  The fiber $S^1$ has known cohomology groups, $H^*(S^1; \\mathbb{Z}) = \\Lambda[e]$.  We know that, $\\mathbb{C}\\mathbb{P}^\\infty$ is connected, and hence we can compute, $H^0(\\mathbb{C}\\mathbb{P}^\\infty; H^* S^1)$ --- it has two free, generators $1$ and $e$ in $q$-degrees $0$ and $1$.";
    ss#open_page ();
    ss#add_object ~name:"one" ~x:0 ~y:0 ~glyph:"\\mathbb{Z}" ~highlighted:false
        ~label:None;
    ss#add_object ~name:"e" ~x:0 ~y:1 ~glyph:"\\mathbb{Z}" ~highlighted:true
        ~label:(Some {str = "e"; direction = LeftOf; distance = None});
    ss#close_page ();
    ()

let stage2 ss =
    ss#render_description "The Serre spectral sequence associated to a singular
    theory is a first-quadrant spectral sequence, and hence $E_2^{p, q} = 0$
    whenever $p$ or $q$ is negative.  The differentials have the type signature
    \\[d_r: E_r^{p, q} \to E_r^{p+r, q-r+1}\\] and hence if the class $e$ is to be
    killed by a differential --- and it must, since $H^*(S^\\infty, \\mathbb{Z})
    = \\mathbb{Z}$ --- it must happen on this page.  Therefore, there must be a
class $x$ in $E_2^{2, 0} = H^2(\\mathbb{C}\\mathrm{P}^\\infty; H^0(S^1;
\\mathbb{Z}))$ with $d_2(e) = x$.";
    ss#open_page ();
    ss#add_object ~name:"x" ~x:2 ~y:0 ~glyph:"\\mathbb{Z}" ~highlighted:true
        ~label:(Some {str = "x"; direction = Below; distance = None});
    ss#add_arrow ~source:"e" ~target:"x" ~highlighted:true ~label:None;
    ss#close_page ();
    ()

let stage3 ss =
    ss#render_description "But, if $E_2^{2, 0} =
        H^2(\\mathbb{C}\\mathrm{P}^\\infty; H^0(S^1; \\mathbb{Z}))$ is nonzero,
        then $E_2^{2, 1} = H^2(\\mathbb{C}\\mathrm{P}^\\infty; H^1(S^1;
        \\mathbb{Z}))$ is also nonzero, since $H^0(S^1; \\mathbb{Z}) \\cong
        H^1(S^1; \\mathbb{Z}) \\cong \\mathbb{Z}$.  The Serre spectral sequence
        is multiplicative, and so we already have a name for this element: $e
        \\cdot x$.  Moreover, $d_2$ is a derivation, so \\begin{align*} d_2(e
        \\cdot x) & = d_2(e) \\cdot x + (-1) e \\cdot d_2(x) \\ & = x^2 + 0 =
        x^2. \\end{align*}  For degree reasons, $e \\cdot x$ must also be killed
        on the $E_2$-page, and hence $x^2$ must exist in $E_2^{4, 0}$.  This
        pattern continues, as $d_2(e \\cdot x^n) = x^{n+1} + (-1) e \\cdot n
        x^{n-1} \\cdot 0 = x^{n+1}$.";
    ss#open_page ();
    (* add more objects... *)
    for i = 2 to 4 do
        let xname = "x" ^ (string_of_int i) in
        let xlabel = "x^" ^ (string_of_int i) in
        let ename = "ex" ^ (string_of_int (i-1)) in
        ss#add_object ~name:xname ~x:(2*i) ~y:0 ~glyph:"\\mathbb{Z}"
            ~highlighted:true ~label:
            (Some {str = xlabel; direction=Below; distance=None});
        ss#add_object ~name:ename ~x:(2*i-2) ~y:1 ~glyph:"\\mathbb{Z}"
            ~highlighted:true ~label:None;
        ss#add_arrow ~source:ename ~target:xname ~highlighted:true ~label:None
    done;
    ss#add_object ~name:"ex4" ~x:8 ~y:1 ~glyph:"\\mathbb{Z}"
        ~highlighted:true ~label:None;
    ss#close_page ();
    ()

let stage4 ss =
    ss#render_description "To build the $E_3$ page, we take cohomology with the
    $d_2$ differentials, and we find nothing left but $1$ in the spectral
    sequence.  Hence, $E_3 \\cong E_\\infty$, and the spectral sequence
    collapses at $E_3$.

    Recall that $E_2^{p, 0} = H^p(\\mathbb{C}\\mathrm{P}^\\infty; H^0(S^1;
    \\mathbb{Z})) = H^p(\\mathbb{C}\\mathrm{P}^\\infty; \\mathbb{Z})$.  So, we
    can now read off the cohomology of $\\mathbb{C}\\mathrm{P}^\\infty$,
    together with its ring structure: \\[H^*(\\mathbb{C}\\mathrm{P}^\\infty;
    \\mathbb{Z}) \\cong \\mathbb{Z}[x],\\] where $|x| = 2$.";
    (* deletion has to happen before we prerender the page! *)
    ss#delete_object "x";
    ss#delete_object "e";
    ss#delete_arrow "e" "x";
    for i = 2 to 4 do
        let xname = "x" ^ (string_of_int i) in
        let ename = "ex" ^ (string_of_int (i-1)) in
        ss#delete_object xname;
        ss#delete_object ename;
        ss#delete_arrow ename xname
    done;
    ss#delete_object "ex4";
    ss#open_page ();
    ss#close_page ();
    ()

let output_cpinfty fh =
    (* initialize the spectral sequence *)
    let ss = new ss ~width:8 ~height:5 ~quadrant:FirstQuadrant
                    ~horizontal_axis_label:"p" ~vertical_axis_label:"q" ~fh in

    (* run through its stages *)
    List.iter (fun x -> x ss) [stage1; stage2; stage3; stage4]
