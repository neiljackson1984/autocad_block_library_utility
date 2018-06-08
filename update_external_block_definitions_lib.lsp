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


;; generates the list (0 1 2 3 ... (rangeSize - 1))
(defun range (rangeSize /
	i
	returnValue
	)
	(setq returnValue (list))
	(setq i 0)
	(while (< i rangeSize) 
		(setq returnValue (append returnValue (list i)))
		(setq i (1+ i))
	)
	returnValue
)


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
			
			
			;;there are cases where attsync will change the value of an attribute.  Specifically, it seems that when an attribute value contains field codes, ATTSYNC, has a tendency to set the value of that attribute to an empty string.
			;; to work around this, we need to record the initial values of the attributes, and then reset them after the attsync.  STUPID!
			
			
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
;; all of this is a work-around for xrefs not supporting attributes or dynamic properties -- irritating.

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

;;this function takes a string and returns a version of that string that is guaranteed not to have any characters that are illegal for autocad names.
(defun sanitizeName (x /
	illegalCharacters
	replacementCharacters
	sanitizedString
	)
	(setq illegalCharacters 
		(list
			"<"
			">"
			"/"
			"\\"
			"\""
			":"
			";"
			"?"
			"*"
			"|"
			","
			"="
			"`"
		)
	)
	(setq replacementCharacters 
		(mapcar '(lambda (x) "-")  illegalCharacters)
	)
	(setq sanitizedString 
		(vl-string-translate (apply 'strcat illegalCharacters) (apply 'strcat replacementCharacters) x)
	)
	sanitizedString
)


;; thanks to https://www.theswamp.org/index.php?topic=41820.0
(defun GUID  (/ tl g)
  (if (setq tl (vlax-get-or-create-object "Scriptlet.TypeLib"))
    (progn (setq g (vlax-get tl 'Guid)) (vlax-release-object tl) (substr g 2 36)))
)
	


(defun update_external_block_definitions
	(	
		destinationDatabase
		blockDefinitionsDirectory
		/
		absolutePathToFile
		attributeReference
		attributeValuesToRestore
		attributeValueToRestore
		blockDefinition
		blockDefinitionIsModelSpace
		blockName
		blockNamesToImport
		blockReference
		container
		destinationBlockDefinitionNew
		destinationBlockDefinitionOld
		dynamicProperties
		entity
		file
		goodValue
		importModelSpaceAsABlockDefinition
		nameOfModelSpace
		sourceBlockDefinition
		sourceDatabase
		tabPrefix
		tempBlockName
		uniquifyingSuffix
		blockReferenceHasAttributes
		mLeader
		theTable
		rowIndex
		columnIndex
		contentIndex
		e
		initialAttributes
		initialAttribute
		attributeDefinition
		initialCellState
	)
	(setq nameOfModelSpace "*Model_Space")
	; (setq uniquifyingSuffix (GUID )) ; we will append this suffix to produce a temporary name.
	(setq uniquifyingSuffix 
		(sanitizeName 
			(rtos 
				(* (getvar 'CDATE) (expt 10 7))
				2 ;;mode (2 means decimal)
				0 ;;precision (i.e. number of decimal places)
			)
		)
	) ; we will append this suffix to produce a temporary name.  ;;it is important to specify the mode argument decimal in rtos because, if we happen to be in architectural number opmde, we could end up with quotes and apostrophes.

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
			;collect a list of the names of the block definitions that we are to import from the source database.
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
					(princ "\n")
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
				(setq attributeValuesToRestore (list )) ;; an associative list whose keys are attributeReference objects and whose values are the values that we want to set to the TextString property after performing ATTSYNC)
				(if destinationBlockDefinitionOld 
					(progn
						(vlax-for container (vla-get-blocks destinationDatabase)
							(vlax-for entity container
								
								
								;; handle the case of a block reference pointing to the old block definition
								(if
									(and
										(= "AcDbBlockReference" (vla-get-ObjectName entity))
										(= (vla-get-Name destinationBlockDefinitionOld) (vla-get-EffectiveName entity))
									)
									(progn
										(setq blockReference entity)
										; (princ "Found a blockReference, owned by ")(princ (vla-get-name (vla-ObjectIDToObject (vla-get-Document blockReference) (vla-get-OwnerID blockReference))))(princ ", pointing to the old definition of the block named ")(princ blockName)(princ ".\n")
										
										;; handle the case where the block is dynamic, in which case we need to take pains to ensure that the property values do not change.  
										;; The property values are reset, by defalt, by calling (vla-put-Name ) onthe block reference.  This is undesirable behavior.
										; (setq wasADynamicBlock (= (vla-get-IsDynamicBlock blockReference) :vlax-true))
										(setq dynamicProperties  (LM:getdynprops blockReference)) ;;if the block reference is not dynamic, this simply returns nil - no exceptions are thrown so we don't need to bother checking whether the blockReference is dynamic.
										(vla-put-Name blockReference (vla-get-Name destinationBlockDefinitionNew)) ;; point the block reference to the new block definition.
										; (if (/= (= (vla-get-IsDynamicBlock blockReference) :vlax-true) wasADynamicBlock)
											; (progn 
												; (princ "\t\t\t")(princ "dynamicness changed from ")(princ wasADynamicBlock)(princ " to ")(princ (= (vla-get-IsDynamicBlock blockReference) :vlax-true)) (princ ".")(princ "\n")
											; )
										; )
										
										(if 
											(and 
												(> (length dynamicProperties) 0) 
												(= (vla-get-IsDynamicBlock blockReference) :vlax-true)
											) ;;we don't strictly need to do this check because none of the stuff that we do within this if statement would cause problems, even if the block were not dynamic.  
											;; we perform the check simply to avoid the time penalty that the below actions incur when we don't truly need to do them.
											(progn
												(princ "\t\t\t")(princ "re-asserting dynamic properties: ")(princ dynamicProperties)(princ "\n")
												;;(vla-Update blockReference) ;; it turns out that it is not necessary to invoke 'Update' before running LM:setdynprops.		
												(LM:setdynprops blockReference dynamicProperties)
												(princ "checkpoint1\n")
												;; for reasons I do not fully understand, the modification of DynamicBlockReferenceProperty::Value (which is what happens within LM:setdynprops) puts the 
												;; block reference into a state where the next running of the ATTSYNC command on the corresponding block definition will cause any attribute references whose values
												;; contain field codes to be set to an empty string.
												;; to work around this problem, we, after running setdynprops but before we have run attsync, walk through the attributeReferences,
												;; if any, and re-set the TextString property of each, using the LM:fieldcodes function to extract the 'real' field codes from the attribute Reference. 
												;; (I suspect this problem is related to ATTSYNC not properly handling anonymous blocks)
												
												;;collect the attributeValues to be restored.
												

												;(setq blockReferenceHasAttributes (= (vla-get-HasAttributes entity) :vlax-true )) 
												; unfortuntely, the HasAttributes property is not a reliable predictor of whether the block reference has attribute references (I think the value of hasAttributes gets set when the block refernce is first created and then persists even if the block is redefined to not have attrivute definitions and the attribute references are deleted).
												; all of this rigamarrol could be fixed by modifying the gc:VariantToLispData function to be ablte
												; to handle empty arrays (i.e. ubound < lbound), and return an empty list, rather than to throw an error as happens now.
												
												(setq blockReferenceHasAttributes
													(and
														(= (type (vla-GetAttributes blockReference)) 'VARIANT)
														(= (type (vlax-variant-value (vla-GetAttributes blockReference))) 'SAFEARRAY)
														(= (vlax-safearray-get-dim (vlax-variant-value (vla-GetAttributes blockReference))) 1)
														(>= 
															(vlax-safearray-get-u-bound (vlax-variant-value (vla-GetAttributes blockReference)) 1)
															(vlax-safearray-get-l-bound (vlax-variant-value (vla-GetAttributes blockReference)) 1)
														)
													)
												)
												
												(if blockReferenceHasAttributes
													(progn
														(foreach attributeReference (gc:VariantToLispData (vla-GetAttributes blockReference))
															(if (setq code (LM:fieldcode (vlax-vla-object->ename attributeReference))) ;;LM:fieldcode returns nil if the attributeReference value contains no field codes, and otherwise returns the entire value of the attributeReference, including the field codes (and, of course, mtext formatting codes, which are indepenedent from field codes.)
																(progn
																	(princ "recording attributeReference value: ")(princ code)(princ "\n")
																	;;(vla-put-TextString attributeReference code)
																	(appendTo 'attributeValuesToRestore 
																		(cons attributeReference code)
																	) 
																)
															)
														)
													)
												)

												
											)
										)
																		
										; ;; I suspect that the above strategy of running LM:getdynprops before update and LM:setdynprops after update does not handle some cases of dynamic blocks (visibility parameters, for instance), but it is better than nothing for now.
										; ;; TO DO: handle the  dynamic block issues that the above strategy does not handle.
										
										
									)
								)

								;; handle the case of an mleader pointing to the old block definition  this case has to be handled specially because an mleader using a block does not produce a blockReference.
								(if
									(and
										(= "AcDbMLeader" (vla-get-ObjectName entity))
										(= (vla-get-ContentType entity) acBlockContent)
										(= (vla-get-ContentBlockType entity) acBlockUserDefined)
										(= (vla-get-Name destinationBlockDefinitionOld) (vla-get-ContentBlockName  entity))
									)
									(progn
										(setq mLeader entity)
										; (princ "Found an mleader, owned by ")(princ (vla-get-name (vla-ObjectIDToObject (vla-get-Document mLeader) (vla-get-OwnerID mLeader))))	(princ ", pointing to the old definition of the block named ")(princ blockName)(princ "\n")
										(vla-put-ContentBlockName mLeader blockName)
									)
								)
								
								;; handle the case of a table cell containing a block reference.  For some incrompehensible reason, a table cell containing a block does not create a block reference; the system for table cells usig blocks is independent of the usual blockReference system.
								(if
									(and
										(= "AcDbTable" (vla-get-ObjectName entity))
									)
									(progn
										(setq theTable entity)
										;; search through all the content items to see if any of them is a block-containing content item
										(foreach rowIndex (range (vla-get-rows theTable))
											; (princ "row ") (princ rowIndex)(princ ":")(princ "\n")
											; (princ "\t")(princ "(vla-GetRowType theTable rowIndex): ")(princ (cdr (assoc (vla-GetRowType theTable rowIndex) AcRowType_enumValues)))(princ "\n")

											; (princ "\t")(princ "cells: ")(princ "\n")
											(foreach columnIndex (range (vla-get-columns theTable))
												; (princ "\t\t")(princ "cell ")(princ "(")(princ rowIndex)(princ " ")(princ columnIndex)(princ ")")(princ ": ")                                                                               (princ "\n")												
												; step through all content items in this cell.
												(setq contentIndex 0)
												(while 
													(not
														;; the below expression is a test for the content item being non-existent.
														(and
															(= (vlax-variant-type (vla-GetValue theTable rowIndex columnIndex contentIndex)) vlax-vbEmpty)
															; vla-GetValue will return an empty variant in the case where the content item does not exist, 
															; but it also returns an empty variant in the case of a non-existent content item.  Fortunately, 
															; a non-existent content item will have, characteristically, GetValue being vlax-vbEmpty and 
															; GetBlockTableRecordId2 being zero.
															(= (vla-GetBlockTableRecordId2 theTable rowIndex columnIndex contentIndex) 0	)
														)
													)
													
													(if  ; if this content item contains a block reference pointing to our block definition...
														(and
															(/= 0 (vla-GetBlockTableRecordId2 theTable rowIndex columnIndex contentIndex))
															(= 
																(vla-get-Name 
																	(vla-ObjectIDToObject (vla-get-Document theTable) 
																		(vla-GetBlockTableRecordId2 theTable rowIndex columnIndex contentIndex)
																	)
																) 
																(vla-get-Name destinationBlockDefinitionOld) 
															)
														)
														(progn
														
															(princ 
																(strcat
																	"Found a reference to the block definition " blockName " at "
																	(vla-get-name container) "/" 
																	(vla-get-Handle theTable) "(table)" 
																	"/"
																	"row" (itoa rowIndex) "," "column" (itoa columnIndex) "," "content" (itoa contentIndex) 
																	"."
																	"\n"
																)
															)
															
															;;record the initial state of the cell's content-locked flag, so that we can restore it when we are done (we will be temporarily turning off content lock while we do the work).
															(setq initialCellState (vla-GetCellState theTable rowIndex columnIndex))
															
															; invoke SetCellState to ensure that the content is not locked.  (If the conent is locked when we try to change the content bleow, an exception is thrown.
															(vla-SetCellState theTable rowIndex columnIndex acCellStateNone) 
															
																
															
															
															;; deal with attribute values.  Is it possible that we can get away without explicitly dealing with atttribute values? 
															; Curiously, we seemed to be able to get away without thinking about attribute values in the case of an Mleader referring to a block 
															; definition; However, in the case of a table content item referring to a block definition, if we don't do anything about attribute values,
															; then all the attribute values get reset to defaults when we re-point the content item to point to the new block definition.
															;populate initialAttributes, a set of name value pairs.
															; Unfortunately, we cannot use the same procedure that we used in the case of regular block references, because block references in a table cell are not ture blockReference objects and do not have true attributeReference objects.
															(setq initialAttributes (list))
															(vlax-for e destinationBlockDefinitionOld
																(if 
																	(= (vla-get-ObjectName e) "AcDbAttributeDefinition")
																	(progn
																		(setq attributeDefinition e)
																		(setq initialAttributes
																			(append
																				initialAttributes
																				(list
																					(list
																						(cons "name" (vla-get-TagString attributeDefinition))
																						(cons "value" 
																							(vla-GetBlockAttributeValue2 theTable rowIndex columnIndex contentIndex
																								(vla-get-ObjectID attributeDefinition)
																							)
																						)
																					)
																				)
																			)
																		)
																	)
																)
															)
															(princ "initialAttributes: ")(princ initialAttributes)(princ "\n")
															

															(vla-SetBlockTableRecordId2 theTable 
																rowIndex columnIndex contentIndex
																(vla-get-ObjectID destinationBlockDefinitionNew)
																(vla-GetAutoScale2 theTable rowIndex columnIndex contentIndex)
															)
															
															;; re-apply the attribute values
															(foreach initialAttribute initialAttributes
															
																; find the attributeDefinition within the new blockDefinition that has the proper TagString
																(setq attributeDefinition nil)
																(vlax-for e destinationBlockDefinitionNew
																	(if 
																		(and
																			(not attributeDefinition)
																			(= (vla-get-ObjectName e) "AcDbAttributeDefinition")
																			(= (vla-get-TagString e) (cdr (assoc "name" initialAttribute)))
																		)
																		(progn
																			(setq attributeDefinition e)
																		)
																	)
																)
																
																(if attributeDefinition
																	(progn
																		(vla-SetBlockAttributeValue2 theTable rowIndex columnIndex contentIndex
																			(vla-get-ObjectID attributeDefinition)
																			(cdr (assoc "value" initialAttribute))
																		)
																	)
																)
															)
															
															;;restore the original content/formatting locking state, which we might have changed above in order to ensure that the content was unlocked.
															(vla-SetCellState theTable rowIndex columnIndex initialCellState) 
														)
													)
													(setq contentIndex (1+ contentIndex))
												)
											)
										)
									)
								)
								
							)	
						)
                        
                        ;;TO DO: handle the case of a tableStyle (really, a TableStyle's TableTemplate) referring to the block
                        ;; unfortunately, this will be difficult because I cannot figure out how to work with the templateTable in the same way as a regular table.
                        (if nil (progn ;;TO DO 
                            
                            ;;handle the case of a tableStyle (really, a TableStyle's TableTemplate) referring to the block
                            (vlax-for tableStyle (vla-item (vla-get-Dictionaries destinationDatabase) "ACAD_TABLESTYLE")
                                
                            )
                        ))
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
				(foreach attributeValueToRestore attributeValuesToRestore
					(setq attributeReference (car attributeValueToRestore))
					(setq goodValue (cdr attributeValueToRestore) )
					
					(princ "Re-asserting value of attributeReference: ")(princ goodValue)(princ "\n")
					(vla-put-TextString attributeReference goodValue)
				)
				
				(princ "\n")
			)
			(vlax-release-object sourceDatabase) ;this is probably not strictly necessary, because garbage collection would handle the closing of the source file even if I did not explicitly release the object.
		)	
	)
	(princ)
)
;===========

(defun c:update_external_block_definitions
	( 
		/
		_blockDefinitionsDirectory
	)
	(setq _blockDefinitionsDirectory
		(if blockDefinitionsDirectory blockDefinitionsDirectory (findfile "block_definitions"))
	)
	(update_external_block_definitions
		(vla-get-ActiveDocument (vlax-get-acad-object)) ; destinationDatabase
		_blockDefinitionsDirectory ; blockDefinitionsDirectory
	)
	(princ)
)


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

;; Copied verbatim from http://www.lee-mac.com/dynamicblockfunctions.html 
;;


;; Get Dynamic Block Property Value  -  Lee Mac
;; Returns the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)

(defun LM:getdynpropvalue ( blk prp )
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)












;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil

(defun LM:setdynpropvalue ( blk prp val )
    (setq prp (strcase prp))
    (vl-some
       '(lambda ( x )
            (if (= prp (strcase (vla-get-propertyname x)))
                (progn
                    (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                    (cond (val) (t))
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)









;; Get Dynamic Block Properties  -  Lee Mac
;; Returns an association list of Dynamic Block properties & values.
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [lst] Association list of ((<prop> . <value>) ... )

(defun LM:getdynprops ( blk )
    (mapcar '(lambda ( x ) (cons (vla-get-propertyname x) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)








;; Set Dynamic Block Properties  -  Lee Mac
;; Modifies values of Dynamic Block properties using a supplied association list.
;; blk - [vla] VLA Dynamic Block Reference object
;; lst - [lst] Association list of ((<Property> . <Value>) ... )
;; Returns: nil

(defun LM:setdynprops ( blk lst / itm )
    (setq lst (mapcar '(lambda ( x ) (cons (strcase (car x)) (cdr x))) lst))
    (foreach x (vlax-invoke blk 'getdynamicblockproperties)
        (if (setq itm (assoc (strcase (vla-get-propertyname x)) lst))
            (vla-put-value x (vlax-make-variant (cdr itm) (vlax-variant-type (vla-get-value x))))
        )
    )
)














;; Get Dynamic Block Property Allowed Values  -  Lee Mac
;; Returns the allowed values for a specific Dynamic Block property.
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; Returns: [lst] List of allowed values for property, else nil if no restrictions

(defun LM:getdynpropallowedvalues ( blk prp )
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)















;; Toggle Dynamic Block Flip State  -  Lee Mac
;; Toggles the Flip parameter if present in a supplied Dynamic Block.
;; blk - [vla] VLA Dynamic Block Reference object
;; Return: [int] New Flip Parameter value

(defun LM:toggleflipstate ( blk )
    (vl-some
       '(lambda ( prp / rtn )
            (if (equal '(0 1) (vlax-get prp 'allowedvalues))
                (progn
                    (vla-put-value prp (vlax-make-variant (setq rtn (- 1 (vlax-get prp 'value))) vlax-vbinteger))
                    rtn
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)

















;; Get Visibility Parameter Name  -  Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil

(defun LM:getvisibilityparametername ( blk / vis )  
    (if
        (and
            (vlax-property-available-p blk 'effectivename)
            (setq blk
                (vla-item
                    (vla-get-blocks (vla-get-document blk))
                    (vla-get-effectivename blk)
                )
            )
            (= :vlax-true (vla-get-isdynamicblock blk))
            (= :vlax-true (vla-get-hasextensiondictionary blk))
            (setq vis
                (vl-some
                   '(lambda ( pair )
                        (if
                            (and
                                (= 360 (car pair))
                                (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                            )
                            (cdr pair)
                        )
                    )
                    (dictsearch
                        (vlax-vla-object->ename (vla-getextensiondictionary blk))
                        "ACAD_ENHANCEDBLOCK"
                    )
                )
            )
        )
        (cdr (assoc 301 (entget vis)))
    )
)













;; Get Dynamic Block Visibility State  -  Lee Mac
;; Returns the value of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Value of Visibility Parameter, else nil

(defun LM:getvisibilitystate ( blk )
    (LM:getdynpropvalue blk (LM:getvisibilityparametername blk))
)






















;; Set Dynamic Block Visibility State  -  Lee Mac
;; Sets the Visibility Parameter of a Dynamic Block (if present) to a specific value (if allowed)
;; blk - [vla] VLA Dynamic Block Reference object
;; val - [str] Visibility State Parameter value
;; Returns: [str] New value of Visibility Parameter, else nil

(defun LM:SetVisibilityState ( blk val / vis )
    (if
        (and
            (setq vis (LM:getvisibilityparametername blk))
            (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
        )
        (LM:setdynpropvalue blk vis val)
    )
)

;; Field Code  -  Lee Mac
;; Returns the field expression associated with an entity

(defun LM:fieldcode ( ent / replacefield replaceobject fieldstring enx )

    (defun replacefield ( str enx / ent fld pos )
        (if (setq pos (vl-string-search "\\_FldIdx" (setq str (replaceobject str enx))))
            (progn
                (setq ent (assoc 360 enx)
                      fld (entget (cdr ent))
                )
                (strcat
                    (substr str 1 pos)
                    (replacefield (fieldstring fld) fld)
                    (replacefield (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
                )
            )
            str
        )
    )

    (defun replaceobject ( str enx / ent pos )
        (if (setq pos (vl-string-search "ObjIdx" str))
            (strcat
                (substr str 1 (+ pos 5)) " "
                (LM:ObjectID (vlax-ename->vla-object (cdr (setq ent (assoc 331 enx)))))
                (replaceobject (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
            )
            str
        )
    )

    (defun fieldstring ( enx / itm )
        (if (setq itm (assoc 3 enx))
            (strcat (cdr itm) (fieldstring (cdr (member itm enx))))
            (cond ((cdr (assoc 2 enx))) (""))
        )
    )
    
    (if (and (wcmatch  (cdr (assoc 0 (setq enx (entget ent)))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION")
             (setq enx (cdr (assoc 360 enx)))
             (setq enx (dictsearch enx "ACAD_FIELD"))
             (setq enx (dictsearch (cdr (assoc -1 enx)) "TEXT"))
        )
        (replacefield (fieldstring enx) enx)
    )
)

;; ObjectID  -  Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems

(defun LM:ObjectID ( obj )
    (eval
        (list 'defun 'LM:ObjectID '( obj )
            (if
                (and
                    (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
                    (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                )
                (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:ObjectID obj)
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)


(defun appendTo 
	(
		theList
		theElementToAppend
		/
		returnValue
	)
	(COND
		(
			(= (type theList) 'SYM)
			(progn
				(if (not (eval theList)) (set theList (list))) ; if theList is undefined, set it to an empty list (I realize that this is a tautology in lisp, since an undefined variabel has value nil, which is the same as an empty list. -- oh well)
				(set theList
					(append
						(eval theList)
						(list theElementToAppend)
					)
				)
				(setq returnValue (eval theList))
			)
		)
		(
			(= (type theList) 'LIST)
			(progn
				(setq returnValue 						
					(append
						(eval theList)
						(list theElementToAppend)
					)
				)
			)
		)
	)
	returnValue
)
(princ)