@echo Compiling OCaml modules...

ocamlc -c SS.ml
@IF %ERRORLEVEL% NEQ 0 Goto End

ocamlc -c SSSBSU.ml
@IF %ERRORLEVEL% NEQ 0 Goto End

ocamlc -c SSSCPinfty.ml
@IF %ERRORLEVEL% NEQ 0 Goto End

ocamlc SS.cmo SSSBSU.cmo SSSCPinfty.cmo Run.ml
@IF %ERRORLEVEL% NEQ 0 Goto End

@echo ... done.



@echo Generating TeX code...
camlprog
@IF %ERRORLEVEL% NEQ 0 Goto End
@echo ... done.



@echo Compiling TeX code...
pdflatex main
@IF %ERRORLEVEL% NEQ 0 Goto End
@echo ... done.

@Goto RealEnd


:End
@echo Error!
:RealEnd
