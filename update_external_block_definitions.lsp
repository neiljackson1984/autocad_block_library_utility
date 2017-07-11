(load "update_external_block_definitions_lib.lsp")

(progn
		
		(setq thisDocument (vla-get-ActiveDocument (vlax-get-acad-object)))
		(setq blockDefinitionsDirectory (findfile "block_definitions"))
		(update_external_block_definitions
			thisDocument ; destinationDatabase
			blockDefinitionsDirectory ; blockDefinitionsDirectory
		)
	(princ)
)
