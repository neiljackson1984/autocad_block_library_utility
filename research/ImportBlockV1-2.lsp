;;-------------------------=={ Import Block }==-------------------------;;
;;                                                                      ;;
;;  This program allows a user to import a block from a selected        ;;
;;  drawing into the active drawing, without opening the external file. ;;
;;                                                                      ;;
;;  Upon calling the program with 'IB' at the command line, the user    ;;
;;  is prompted to select a drawing file from which to source the       ;;
;;  block to be imported.                                               ;;
;;                                                                      ;;
;;  Following a valid response, a dialog is displayed prompting the     ;;
;;  user to select a block from the list of blocks defined in the       ;;
;;  selected drawing and not already present in the current drawing.    ;;
;;                                                                      ;;
;;  Following selection, the definition of the chosen block is imported ;;
;;  into the drawing and the user is prompted to specify a block        ;;
;;  insertion point at which a block reference of the imported block    ;;
;;  is inserted.                                                        ;;
;;                                                                      ;;
;;  The program will perform successfully in all UCS/Views and is also  ;;
;;  compatible with Dynamic Blocks.                                     ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    16-06-2013                                      ;;
;;----------------------------------------------------------------------;;

(defun c:ib ( / *error* abc blk dbx dch dcl des doc dwg ins itm lst )

    (defun *error* ( msg )
        (if (and (= 'vla-object (type dbx)) (not (vlax-object-released-p dbx)))
            (vlax-release-object dbx)
        )
        (if (= 'file (type des))
            (close des)
        )
        (if (and (= 'str (type dcl)) (findfile dcl))
            (vl-file-delete dcl)
        )
        (if (< 0 dch)
            (unload_dialog dch)
        )
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (cond
        (   (not (setq dwg (getfiled "Select Source Drawing" "" "dwg;dwt;dws" 16)))
            (princ "\n*Cancel*")
        )
        (   (= (strcase dwg) (strcase (vla-get-fullname (setq doc (vla-get-activedocument (vlax-get-acad-object))))))
            (princ "\nCannot import from current drawing.")
        )
        (   (not (setq dbx (LM:GetDocumentObject dwg)))
            (princ "\nUnable to interface with selected drawing.")
        )
        (   (progn
                (vlax-for def (vla-get-blocks dbx)
                    (if
                        (and
                            (= :vlax-false (vla-get-isxref   def))
                            (= :vlax-false (vla-get-islayout def))
                            (not (wcmatch (vla-get-name def) "`**,*|*"))
                        )
                        (setq lst (cons (vla-get-name def) lst))
                    )
                )
                (not (setq lst (vl-sort (vl-remove-if '(lambda ( x ) (tblsearch "block" x)) lst) '<)))
            )
            (princ "\nNo distinct blocks found in selected drawing.")
        )
        (   (not
                (and
                    (setq dcl (vl-filename-mktemp nil nil ".dcl"))
                    (setq des (open dcl "w"))
                    (write-line "importblock : dialog { label = \"Select Block to Import\"; spacer;" des)
                    (write-line ": list_box { key = \"lst\"; } spacer; ok_cancel; }" des)
                    (not (setq des (close des)))
                    (< 0 (setq dch (load_dialog dcl)))
                    (new_dialog "importblock" dch)
                )
            )
            (princ "\nUnable to load \"Import Block\" dialog.")
        )
        (   t
            (start_list "lst")
            (foreach itm lst (add_list itm))
            (end_list)

            (set_tile    "lst"  (setq itm "0"))
            (action_tile "lst" "(setq itm $value)")

            (if (= 1 (start_dialog))
                (if
                    (and
                        (not
                            (vl-catch-all-error-p
                                (vl-catch-all-apply 'vlax-invoke
                                    (list dbx 'copyobjects
                                        (list (LM:getitem (vla-get-blocks dbx) (setq blk (nth (atoi itm) lst))))
                                        (setq abc (vla-get-blocks doc))
                                    )
                                )
                            )
                        )
                        (LM:getitem abc blk)
                    )
                    (if (setq ins (getpoint "\nSpecify point for block: "))
                        (vla-insertblock
                            (vlax-get-property doc (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                            (vlax-3D-point (trans ins 1 0))
                             blk
                             1.0 1.0 1.0
                            (angle '(0.0 0.0) (trans (getvar 'ucsxdir) 0 (trans '(0.0 0.0 1.0) 1 0 t) t))
                        )
                        (princ "\nUnable to import block from selected drawing.")
                    )
                )
                (princ "\n*Cancel*")
            )
        )
    )
    (*error* nil)
    (princ)
)

;; Get Document Object  -  Lee Mac
;; Retrieves the VLA Document Object for the supplied filename.
;; The Document Object may be present in the Documents collection, or obtained through ObjectDBX.
;; It is the callers responsibility to release such object.

(defun LM:GetDocumentObject ( dwg / app dbx dwl vrs )
    (cond
        (   (not (setq dwg (findfile dwg))) nil)
        (   (cdr
                (assoc (strcase dwg)
                    (vlax-for doc (vla-get-documents (setq app (vlax-get-acad-object)))
                        (setq dwl (cons (cons (strcase (vla-get-fullname doc)) doc) dwl))
                    )
                )
            )
        )
        (   (progn
                (setq dbx
                    (vl-catch-all-apply 'vla-getinterfaceobject
                        (list app
                            (if (< (setq vrs (atoi (getvar 'acadver))) 16)
                                "objectdbx.axdbdocument" (strcat "objectdbx.axdbdocument." (itoa vrs))
                            )
                        )
                    )
                )
                (or (null dbx) (vl-catch-all-error-p dbx))
            )
            (prompt "\nUnable to interface with ObjectDBX.")
        )
        (   (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-open (list dbx dwg))))
            dbx
        )
    )
)

;; VLA-Collection: Get Item  -  Lee Mac
;; Retrieves the item with index 'idx' if present in the supplied collection
;; col - [vla]     VLA Collection Object
;; idx - [str/int] Index of the item to be retrieved
 
(defun LM:getitem ( col idx / obj )
    (if (not (vl-catch-all-error-p (setq obj (vl-catch-all-apply 'vla-item (list col idx)))))
        obj
    )
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: ImportBlock.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,$(getvar,date),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Type \"IB\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;