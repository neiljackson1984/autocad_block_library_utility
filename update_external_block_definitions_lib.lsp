;(load "acaddoc.lsp")
;(load "variantArrayConversion.lsp")

(defun setAttribute
	(
		theBlockReference
		tag
		newValue
		/
		theBlockReference
		attributeReferences
		theAttributeReference
	)
	; (setq theBlockReference
		; (vlax-ename->vla-object 
			; (car (entsel "select a block reference"))
		; )
	; )
	(setq attributeReferences
		(gc:VariantToLispData 
			(vla-GetAttributes theBlockReference)
		)
	)
	(foreach x attributeReferences
		(if (= tag (vla-get-TagString x))
			(setq theAttributeReference x)
		)
	)
	
	(princ "\n")
	(princ "theAttributeReference.TextString: ")(princ (vla-get-TextString theAttributeReference))(princ "\n")
	(princ "theAttributeReference.MTextAttributeContent: ")(princ (vla-get-MTextAttributeContent theAttributeReference))(princ "\n")
	;(vlax-dump-object theAttributeReference)
	(vla-put-TextString theAttributeReference newValue)
	
	theAttributeReference	
)
;===========

; runs the attsync command on the specified block definition 
;; (expected to be a block definition object)
(defun attributeSync
	(
		;blockName
		blockDefinition
		/
		originalCmdEcho
		blockDefinitionHasAttributes
		entity
		container
		attributeReference
		entityHasAttributes
	)
	(setq originalCmdEcho (getvar "CMDECHO"))
	(setvar "CMDECHO" 0)
	
	; (setq blockDefinition 
		; (vla-Item 
			; (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) 
			; blockName
		; )
	; )
	
	
	(setq blockDefinitionHasAttributes nil)
	(vlax-for entity blockDefinition
		(if (= "AcDbAttributeDefinition" (vla-get-ObjectName entity))
			(setq blockDefinitionHasAttributes T)
		)
	)
	(if blockDefinitionHasAttributes
		(progn
			(vl-cmdf
				"._ATTSYNC"
				"Name"
				(vla-get-Name blockDefinition)
			)
		)
		(progn	;in this case the block definition does not have any attributes.	
			; It seems that the attsync command will never remove all attribute referenfces from a block reference.
			; You might want to remove all attribute references from a  block reference  in the case where you had removed all
			; attribute definitions from the block definition.
			; Therefore, to make this function complete: if not blockDefinitionHasAttributes, we ought to explicitly delete any attribute references
			; attached to any block references that point to the block definition.
			; The ATTREDEF command sounds promising, but I could not get it to immediately do what I needed.
			;scan through all entities in all block definitions, and for any block references pointing to our blockDefinition,
			; delete any attribute references atached to the block reference.
			(vlax-for container (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
				(vlax-for entity container
					(if
						(and
							(= (vla-get-ObjectName entity) "AcDbBlockReference" )
							(= (vla-get-Name entity)   (vla-get-Name blockDefinition))
						)
						(progn ; in this case, entity is a blockReference pointing to a blockDefinition which does not contain any attribute definitions
							
							;(setq entityHasAttributes (= (vla-get-HasAttributes entity) :vlax-true )) 
							; unfortuntely, the HasAttributes property is not a reliable predictor of whether the block reference has attribute references (I think the value of hasAttributes gets set when the block refernce is first created and then persists even if the block is redefined to not have attrivute definitions and the attribute references are deleted).
							; all of this rigamarrol could be fixed by modifying the gc:VariantToLispData function to be ablte
							; to handle empty arrays (i.e. ubound < lbound), and return an empty list, rather than to throw an error as happens now.
							
							(setq entityHasAttributes
								(and
									(= (type (vla-GetAttributes entity)) 'VARIANT)
									(= (type (vlax-variant-value (vla-GetAttributes entity))) 'SAFEARRAY)
									(= (vlax-safearray-get-dim (vlax-variant-value (vla-GetAttributes entity))) 1)
									(>= 
										(vlax-safearray-get-u-bound (vlax-variant-value (vla-GetAttributes entity)) 1)
										(vlax-safearray-get-l-bound (vlax-variant-value (vla-GetAttributes entity)) 1)
									)
								)
							)
							
							(if entityHasAttributes
								(progn
									(foreach attributeReference (gc:VariantToLispData (vla-GetAttributes entity))
										(vla-Delete attributeReference)
									)
								)
							)
						)
					)										
				)
			)
		)
	)


	; it seems that the attsync command syncs attributes for all the block references pointing to a given block definition,
	; not just one specific block reference.
	(setvar "CMDECHO" originalCmdEcho)
)

;; Get Document Object  -  Lee Mac
;; Retrieves the VLA Document Object for the supplied filename.
;; The Document Object may be present in the Documents collection, or obtained through ObjectDBX.
;; It is the callers responsibility to release such object.
(defun LM:GetDocumentObject ( dwg / app dbx dwl vrs )
    (cond
        (   (not (setq dwg (findfile dwg))) (progn (princ "returning nil\n") nil))
        (   
			;this evaluates to the document object of the specified drawing iff. that drawing is open in autocad, else nil.
			(cdr
                (assoc (strcase dwg)
                    
					;this evaluates to a dotted list whose elements are of the form (<documentName> . <documentObject>)
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

;;(vla-open (vla-getinterfaceobject (vlax-get-acad-object) "objectdbx.axdbdocument.19") "C:\\work\\UWT-USC\\block_definitions\\itg_title_block_30x42.dwg")

;; This function does not do anything with the nested blocks -- nested block references are not
;; relaced (although they should be)
;; all of this is a work-around for xrefs not supporting attributes or dynamic properties -- irritating.insert

;; VLA-Collection: Get Item  -  Lee Mac
;; Retrieves the item with index 'idx' if present in the supplied collection
;; col - [vla]     VLA Collection Object
;; idx - [str/int] Index of the item to be retrieved
 
(defun LM:getitem ( col idx / obj )
    (if (not (vl-catch-all-error-p (setq obj (vl-catch-all-apply 'vla-item (list col idx)))))
        obj
    )
)


(defun reloadBlock
	(
		nameOfBlock
		pathToFile
		/
		originalFileDia
		originalCmdEcho
		acadObj
		doc
		modelSpace
		layers
		groups
		blockDefinitions
		currentSpace
		currentLayer
		currentLayerWasInitiallyLocked
		currentLayerWasInitiallyFrozen
	)
	
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq modelSpace (vla-get-ModelSpace doc))
	(setq paperSpace (vla-get-PaperSpace doc))
	(setq layers (vla-get-Layers doc))
	(setq groups (vla-get-Groups doc))
	(setq blockDefinitions (vla-get-Blocks doc))
	
	(COND
	
		(
			(= acPaperSpace (vla-get-ActiveSpace doc))
			(setq currentSpace paperSpace)
		)
		
		(
			(= acModelSpace (vla-get-ActiveSpace doc))
			(setq currentSpace modelSpace)
		)
		
		(
			T
			(princ "Error: ActiveSpace is neither acModelSpace nor acPaperSpace -- this is very strange.\n")
		)
	)
	
	
	(setq originalFileDia (getvar "FILEDIA"))
	(setq originalCmdEcho (getvar "CMDECHO"))
	(setvar "FILEDIA" 0)
	(setvar "CMDECHO" 0)
	
	(setq currentLayer
		(vla-Item layers (getvar "CLAYER"))
	)
	(setq currentLayerWasInitiallyLocked (= (vla-get-Lock currentLayer) :vlax-true))
	(setq currentLayerWasInitiallyFrozen (= (vla-get-Freeze currentLayer) :vlax-true))
	
	; (princ "\n")
	; (princ "currentLayerWasInitiallyLocked: ")(princ currentLayerWasInitiallyLocked) (princ "\n")
	; (princ "currentLayerWasInitiallyFrozen: ")(princ currentLayerWasInitiallyFrozen) (princ "\n")
	
	; unlock and unfreeze the current layer.
	(if currentLayerWasInitiallyLocked
		(vla-put-Lock currentLayer :vlax-false)
	)
	
	(if currentLayerWasInitiallyFrozen
		(vla-put-Freeze currentLayer :vlax-false)
	)
	
	
	;; TO-DO: handle the case where the block definition does not yet exist.
	(vl-cmdf
		"._-INSERT"
		(strcat nameOfBlock "=" pathToFile)
		;;"Yes" ;;yes, we do want to redefine. ;this was not necessary for some reason
		"-10,-10,0"
		1 ;;x scale factor
		1 ;;y scale factor
		0 ;; rotation
	)
	
	;;delete the block reference that we just inserted.
	(vla-Delete 
		(vlax-ename->vla-object (entlast))
	)
	
	; restore the initial lock and freeze state of the current layer.
	(if currentLayerWasInitiallyLocked 
		(vla-put-Lock currentLayer :vlax-true)
	)
	(if currentLayerWasInitiallyFrozen 
		(vla-put-Freeze currentLayer :vlax-true)
	)
	
	(attributeSync (vla-Item (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) nameOfBlock) )
	
	(setvar "FILEDIA" originalFileDia)
	(setvar "CMDECHO" originalCmdEcho)
)
;===========



(defun update_external_block_definitions
	(	
		/
		uniquifyingSuffix
		blockDefinitionsDirectory
		destinationDatabase
		file
		absolutePathToFile
		sourceDatabase
		blockNamesToImport
		blockDefinition
		blockName
		sourceBlockDefinition
		destinationBlockDefinitionOld
		tempBlockName
		destinationBlockDefinitionNew
		container
		entity
		tabPrefix
		importModelSpaceAsABlockDefinition
		nameOfModelSpace
		blockDefinitionIsModelSpace
	)
	(setq nameOfModelSpace "*Model_Space")
	(setq uniquifyingSuffix "asdfla234985723897154asd") ; we will append this suffix to produce a temporary name.
	(setq blockDefinitionsDirectory (findfile "block_definitions")) ;  the directory containing dwg files, each of which shall be imported into this file as a block definition
	(setq destinationDatabase (vla-get-ActiveDocument (vlax-get-acad-object))) ;;set the destinationDatabase to be the current document
	(if (vl-file-directory-p blockDefinitionsDirectory) 
		(foreach file (vl-directory-files blockDefinitionsDirectory "*.dwg" 1) ; the '1' causes the function to list files only (not folders).
			(setq absolutePathToFile (strcat blockDefinitionsDirectory "\\" file))
			(princ "\n")
			(princ (strcat "Importing block definitions from " absolutePathToFile)) (princ "\n")
			(setq tabPrefix "\t\t") ; to be used to indent subsequent lines of diagnostic message
			
			;=== IMPORT THE FILE ITSELF AS A BLOCK DEFINITION
			;; only if the file is not explicitly designated as a library (with the file name suffix .lib.dwg)
			(if 
				(wcmatch (strcase file) "*.LIB.DWG")
				(progn
					(princ tabPrefix)(princ "This is a library file (filename ends with \".lib.dwg\"), so we will not import the modelSpace as a block definition.")(princ "\n")
					(setq importModelSpaceAsABlockDefinition nil)
				)
				(progn
					(princ tabPrefix)(princ "This is not a library file (filename does not end with \".lib.dwg\"), so we will import the modelSpace as a block definition.")(princ "\n")
					(setq importModelSpaceAsABlockDefinition T)
				)
			)
			
			(if importModelSpaceAsABlockDefinition
				(progn
					; this is a bit of a hack until I can get modelspace block importing figured out using the more elegant CopyObjects function.
					(reloadBlock (vl-filename-base absolutePathToFile) absolutePathToFile)
				)
			)
			
			;=== IMPORT EVERY BLOCK DEFINITION WITHIN THE FILE AS A BLOCK DEFINITION
			(setq sourceDatabase (LM:GetDocumentObject absolutePathToFile))
			;TODO: test whether sourceDatabase is nil, and throw an error if so.
			; collect a list of the names of block definitions to import.
			(if (not sourceDatabase) (princ "Warning: source database could not be loaded."))
			(setq blockNamesToImport (list))
			(vlax-for blockDefinition (vla-get-blocks sourceDatabase)
				(setq blockDefinitionIsModelSpace (= (vla-get-Name blockDefinition) nameOfModelSpace))
				(if
					(or
						(and
							(= :vlax-false (vla-get-isxref   blockDefinition))
							(= :vlax-false (vla-get-islayout blockDefinition))
							(not (wcmatch (vla-get-name blockDefinition) "`**,*|*")) ; we want to exclude any block definitions whose names start with an asterisk or contain a vertical bar (not sure why we care about vertical bar; that is left over from Lee Mac's script and I have not bothered to remove it.)
						)
						(and 
							nil ; importing model space is not working right now, due to an inability to rename the model space block definition, so I am disabling it with this nil.
							importModelSpaceAsABlockDefinition 
							blockDefinitionIsModelSpace
						) ;UNLESS this block definition is model space and we are supposed to import model space.
					)
					(progn
						(setq blockNamesToImport (append blockNamesToImport (list (vla-get-name blockDefinition)))) ;;append the block name to blockNamesToImport
					)
				)
			)
			;at this point, the only artifact of the model space importing flags that we care about is whether the name nameOfModelSpace is a member
			; of blockNamesToImport.
			(foreach blockName blockNamesToImport
				(setq sourceBlockDefinition (vla-Item (vla-get-blocks sourceDatabase) blockName))
				
				(if (= blockName nameOfModelSpace)
					(progn
						(setq blockName (vl-filename-base (vla-get-Name sourceDatabase)))
						(vla-put-Name sourceBlockDefinition blockName) ; I  doubt that this will work -- we are attempting to rename modelspace, which I don't think acad will like.
						(if (not (= (vla-get-Name sourceBlockDefinition) blockName))
							(progn
								(princ "\n") (princ "attempting to rename modelSpace in source database failed.")(princ "\n")
							)
						)
					)
				)
				
				(setq destinationBlockDefinitionOld (LM:getitem (vla-get-blocks destinationDatabase) blockName)) ;; this will be nil if there is no existing block definition of the specified name in the destination database. 
				(setq tempBlockName (strcat blockName uniquifyingSuffix))
				(if destinationBlockDefinitionOld (vla-put-Name destinationBlockDefinitionOld tempBlockName))
				
				(progn ; diagnostic message
					(princ tabPrefix)
					(princ 
						(if destinationBlockDefinitionOld 
							"importing (overwriting) :  "
							"importing               :  "
						)
					)
					(princ blockName)

				)
				
				;; (vla-put-name sourceBlockDefinition tempBlockName)
				;; I am going to use the tempName for the old destination block definition, instead of the sourceBlockDefinition.  Assigning the temp
				;; name to the source definition can cause the temp names to persist after the import in cases where the block definitions in the source database
				;; contain references to other block definitions in the source database.
				
				;perform the copying.
				(vla-CopyObjects 
					sourceDatabase 											; the database whose "CopyObjects" method we are calling (this is the database from which we are copying things)
					(gc:ObjectListToVariant (list sourceBlockDefinition))		; the list of objects to be copied
					(vla-get-blocks destinationDatabase) 					; the owner to whom thses objects will be copied
				)
				(setq destinationBlockDefinitionNew (LM:getitem (vla-get-blocks destinationDatabase) blockName))
				;modify each reference that points to the old block definition to make it point to the new block definition
				(if destinationBlockDefinitionOld 
					(progn
						(vlax-for container (vla-get-blocks destinationDatabase)
							(vlax-for entity container
								(if
									(and
										(= "AcDbBlockReference" (vla-get-ObjectName entity))
										(= (vla-get-Name destinationBlockDefinitionOld) (vla-get-Name entity))
									)
									(progn
										(vla-put-Name entity (vla-get-Name destinationBlockDefinitionNew))
									)
								)										
							)	
						)
					)
				)
				; at this point, there will be no references pointing to the old block definition (because we have just re-pointed each
				; of those references), so we may safely delete the old block definition.
				(if destinationBlockDefinitionOld 
					(progn 
						;(princ " deleting old block definition")
						(vla-Delete destinationBlockDefinitionOld)
					)
				)
				(attributeSync destinationBlockDefinitionNew)
				(princ "\n")
			)
			(vlax-release-object sourceDatabase) ;this is probably not strictly necessary, because garbage collection would handle the closing of the source file even if I did not explicitly release the object.
		)	
	)
	(princ)
)
;===========



;;; copied on 2016/01/17 from http://www.theswamp.org/index.php?topic=31674.5;wap2 

;;;======================== VARIANTS & SAFEARRAYS ========================;;;

;; Variant -> LISP

;; gc:VariantToLispData
;; Converts a variant or a safearray into LISP data (list)
;;
;; Argument: var variant or safearray

(defun gc:VariantToLispData (var)
  (cond
    ((= (type var) 'variant)
     (gc:VariantToLispData (vlax-variant-value var)))
    ((= (type var) 'safearray)
     (mapcar 'gc:VariantToLispData (vlax-safearray->list var))
    )
    (T var)
  )
)

;; gc:2dVariantToPointList
;; Converts a variant of 2D coordinates into a 2d points list
;; LightweightPolyline: OCS coordinates
;;
;; Argument
;; var: a variant (array of doubles) as returned by vla-get-Coordinates

(defun gc:2dVariantToPointList (var / foo)
  (defun foo (lst)
    (if lst
      (cons (list (car lst) (cadr lst)) (foo (cddr lst)))
    )
  )
  (foo (vlax-safearray->list (vlax-variant-value var)))
)

;; gc:3dVariantToPointList
;; Converts a variant of 3D coordinates into a 3d points list
;; 2d Polyline: OCS coordinates (Z = 0)
;; 3DFace, 3DPolyline, Leader, MLine, PolyfaceMesh,
;; PolygonMesh, Solid, Trace: WCS coordinates
;;
;; Argument
;; var: a variant (array of doubles) as returned by vla-get-Coordinates

(defun gc:3dVariantToPointList (var / foo)
  (defun foo (lst)
    (if lst
      (cons (list (car lst) (cadr lst) (caddr lst)) (foo (cdddr lst)))
    )
  )
  (foo (vlax-safearray->list (vlax-variant-value var)))
)

;; gc:VariantsToDxfList
;; Returns an assoc list (DXF list type)
;;
;; Arguments
;; xtyp: variant (array of integers)
;; xval: varinat (array of variants)

(defun gc:VariantsToDxfList (xtyp xval)
  (mapcar 'cons (gc:VariantToLispData xtyp) (gc:VariantToLispData xval))
)

;; gc:GetXdata
;; Returns the object xadta list
;;
;; Arguments
;; obj: (vla-object) the object containing xdata
;; app: (string) the registred application name ("" for all)

(defun gc:GetXdata (obj app / xtyp xval)
  (vla-GetXdata obj app 'xtyp 'xval)
  (gc:VariantsToDxfList xtyp xval)
)

;; gc:GetXrecordData
;; Returns the xrecord object DXF data list
;;
;; Arguments
;; xrec: (vla-object) thet XRECORD object

(defun gc:GetXrecordData (xrec / xtyp xval)
  (vla-GetXrecordData xrec 'xtyp 'xval)
  (gc:VariantsToDxfList xtyp xval)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LISP -> variant

;; gc:2dPointListToVariant (gile)
;; Return a variant of 2d coordinates
;;
;; Argument: a 2d points list -type (x y)-

(defun gc:2dPointListToVariant (lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray
        vlax-VbDouble
        (cons 0 (1- (* 2 (length lst))))
      )
      (apply 'append lst)
    )
  )
)

;; gc:3dPointListToVariant (gile)
;; Return a variant of 3d coordinates
;;
;; Argument: a 3d points list -type (x y z)-

(defun gc:3dPointListToVariant (lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray
        vlax-VbDouble
        (cons 0 (1- (* 3 (length lst))))
      )
      (apply 'append lst)
    )
  )
)

;; gc:ObjectListToVariant
;; returns a variant (array of objects)
;;
;; Argument
;; lst: a vla-object list

(defun gc:ObjectListToVariant (lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray
        vlax-vbObject
        (cons 0 (1- (length lst)))
      )
      lst
    )
  )
)

;; gc:DxfListToVariants
;; Defines 2 variables and bounds a variant to each
;;
;; Arguments
;; lst: a DXF list
;; typeSymbol: a quoted symbol (other than 'typeSymbol)
;; valueSymbol: a quoted symbol (other than 'valueSymbol)

(defun gc:DxfListToVariants (lst typeSymbol valueSymbol)
  (set typeSymbol
       (vlax-make-variant
         (vlax-safearray-fill
           (vlax-make-safearray
             vlax-vbInteger
             (cons 0 (1- (length lst)))
           )
           (mapcar 'car lst)
         )
       )
  )
  (set valueSymbol
       (vlax-make-variant
         (vlax-safearray-fill
           (vlax-make-safearray
             vlax-vbVariant
             (cons 0 (1- (length lst)))
           )
           (mapcar '(lambda (x)
                      (if (listp (setq x (cdr x)))
                        (vlax-3d-point x)
                        (vlax-make-variant x)
                      )
                    )
                   lst
           )
         )
       )
  )
)


;; gc:SetXdata
;; Set xdatas to an object
;;
;; Arguments
;; obj: (vla-object) the object to set xdatas
;; lst: (liste DXF) the xdatas as:
;; '((1001 . "App_Name") (1002 . "{") (1000 . "string") (1070 . 1) (1002 . "}"))

(defun gc:SetXdata (obj lst / xtyp xval)
  (gc:DxfListToVariants lst 'xtyp 'xval)
  (vla-SetXdata obj xtyp xval)
)

;; gc:SetXrecordData
;; Set datas to an xrecord
;;
;; Arguments
;; xrec: (vla-object) the Xrecord object
;; lst : (liste DXF) the datas as:
;; '((1 . "string") (70 . 1) (10 1.0 2.0 0.0))

(defun gc:SetXrecordData (xrec lst / xtyp xval)
  (gc:DxfListToVariants lst 'xtyp 'xval)
  (vla-SetXrecordData xrec xtyp xval)
)