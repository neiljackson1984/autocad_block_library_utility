;Block redefinition - for batch scripts
;replaces <BLKNAME> with a new version of the block saved in <BLKNAME>.DWG
;(DWG file must be on search path)
; 
;(C) 2007-2013 CAD Studio a.s. - www.cadstudio.cz
; 
(defun C:BLKREDEF ( / blkname cmde ss)
 (setq cmde (getvar "cmdecho"))
 (setvar "cmdecho" 0)
 (setq blkname (getstring "\nBlock name ('?' to pick): ")) ; rem
;added 7/2013
  (if (and blkname (= blkname "?"))(progn ; rem
  (prompt "\nPick a block:")
  (setq ss (ssget ":S" '((0 . "INSERT"))))
  (if ss (setq blkname (cdr(assoc 2 (entget(ssname ss 0))))))
  (princ blkname)
 )) ; rem
;--
 (if (tblsearch "block" blkname)
  (progn
;   (command "._-insert" (strcat blkname "=") "_y" nil "_regen") ; breaks scripts...
   (command "._-insert" (strcat blkname "=") "_y" "@")
   (while (= (logand (getvar "CMDACTIVE") 1) 1)(command "")) ; for scripts
   (entdel (entlast))
   (command "_regen")
   (princ (strcat "\nBlock " (strcase blkname) " redefined."))
  )
  (princ "Block not found.")
 )
 (setvar "cmdecho" cmde)
 (prin1)
) 