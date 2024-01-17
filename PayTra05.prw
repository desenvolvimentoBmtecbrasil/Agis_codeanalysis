#include "Totvs.ch"

/*/{Protheus.doc} PayTra05

Inclusao PC

@type function
@author Katieli de Oliviera - BMTEC
@since 04/01/2022
@version P11,P12
@database SQL Server,Oracle

@history 04/01/2022, Liberacao inicial
@history 21/07/2023, Refatorar

@param oParseJSON, objeto, Objeto json com os dados enviados

@return caracter, Erro

@see WsPayTrac01
@see PayTra01
@see PayTra02
@see PayTra05
@see PayTra04
@see PayTra06

/*/

User Function PayTra05(oParse)

	Local oJsonRet  := JsonObject():New()
	Local aMsg		:= {}
	Local aCabec	:= {}
	Local aItens	:= {}
	Local aLinha	:= {}
	Local nX		:= 0
	Local lError    := .F.
	Local cErro     := ""
	Local cError := Space(0)
	Local bError := ErrorBlock({|oError| cError := oError:Description})
	Local cJson 			:= oParse:ToJson()
	Local oParseJSON 		:= Nil

	Private lMsErroAuto := .F.
	Private lAutoErrNoFile := .T.

	U_pFwLog("INFO","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][INFO] - INICIO PROCESSO INCLUSAO DE PC')

	oJsonRet['STATUS'] := JsonObject():New()
	oJsonRet['WARN']   := JsonObject():New()

	FWJsonDeserialize(DecodeUtf8(cJson),@oParseJSON)
	
	if	!AttIsMemberOf(oParseJSON,"C7_FORNECE")  
		lError   := .T.
		cMessage := 'Atributo C7_FORNECE nao informado no corpo da requisicao'
	elseif  !AttIsMemberOf(oParseJSON,"C7_COND")  
		lError   := .T.
		cMessage := 'Atributo C7_COND nao informado no corpo da requisicao'
	elseif  !AttIsMemberOf(oParseJSON,"C7_CONTATO")  
		lError   := .T.
		cMessage := 'Atributo C7_CONTATO nao informado no corpo da requisicao'
	endIf

	if lError
		oJsonRet['STATUS'] := .F.
		oJsonRet['WARN']   := cMessage
		U_pFwLog("ERROR","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][ERROR] - '+cMessage)
		return oJsonRet
	endIf

	dbSelectArea("SA2")
	dbSetOrder(3)

	If !SA2->( dbSeek( xFilial("SA2") + PadR( Alltrim(oParseJSON:C7_FORNECE), TamSX3("A2_CGC")[01] ) ) )
		cMessage           := 'Fornecedor nao cadastrado [CGC]: '+PadR( Alltrim(oParseJSON:C7_FORNECE), TamSX3("A2_CGC")[01] )
		oJsonRet['STATUS'] := .F.
		oJsonRet['WARN']   := cMessage
		U_pFwLog("ERROR","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][ERROR] - '+cMessage)
		return oJsonRet
	EndIf

	dbSelectArea("SE4")
	dbSetOrder(1)

	If !SE4->( dbSeek( xFilial( "SE4" ) + PadR( oParseJSON:C7_COND, TamSX3("C7_COND")[01] ) ) )

		cMessage :='Condicao de pagamento nao cadastrada: '+PadR( oParseJSON:C7_COND, TamSX3("C7_COND")[01] )
		oJsonRet['STATUS'] := .F.
		oJsonRet['WARN']   := cMessage
		U_pFwLog("ERROR","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][ERROR] - '+cMessage)
		return oJsonRet

	EndIf

	aAdd(aCabec,{"C7_EMISSAO" , dDataBase})
	aAdd(aCabec,{"C7_FORNECE" , SA2->A2_COD})
	aAdd(aCabec,{"C7_LOJA"    , SA2->A2_LOJA})
	aAdd(aCabec,{"C7_COND"    , SE4->E4_CODIGO})
	aAdd(aCabec,{"C7_CONTATO" , oParseJSON:C7_CONTATO})
	aAdd(aCabec,{"C7_FILENT"  , cFilAnt})

	If !AttIsMemberOf(oParseJSON,'ITENS')

		For nX := 1 To Len( oParseJSON:itens )

			if !AttIsMemberOf(oParseJSON:itens[nX],"C7_PRODUTO")
				lError   := .T.
				cMessage := 'Atributo C7_PRODUTO nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:itens[nX],"C7_QUANT") 
				lError   := .T.
				cMessage := 'Atributo C7_QUANT nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:itens[nX],"C7_PRECO")
				lError   := .T.
				cMessage := 'Atributo C7_PRECO nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:itens[nX],"C7_OBS")
				lError   := .T.
				cMessage := 'Atributo C7_OBS nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:itens[nX],"C7_CC")
				lError   := .T.
				cMessage := 'Atributo C7_CC nao informado no corpo da requisicao'
			endIf

			if lError
				oJsonRet['STATUS'] := .F.
				oJsonRet['WARN']   := cMessage
				U_pFwLog("ERROR","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][ERROR] - '+cMessage)
				return oJsonRet
			endIf

			aLinha := {}
			aAdd(aLinha,{"C7_PRODUTO"	, oParseJSON:itens[nX]:C7_PRODUTO		    , Nil})
			aAdd(aLinha,{"C7_QUANT"		, oParseJSON:itens[nX]:C7_QUANT			    , Nil})
			aAdd(aLinha,{"C7_PRECO"		, oParseJSON:itens[nX]:C7_PRECO		  	    , Nil})
			aAdd(aLinha,{"C7_OBS"		, DecodeUTF8(oParseJSON:itens[nX]:C7_OBS)	, Nil})
			aAdd(aLinha,{"C7_CC"		, oParseJSON:itens[nX]:C7_CC 			    , Nil})

			aAdd(aItens,aLinha)

		Next nX

		BEGIN SEQUENCE

			MSExecAuto( {|v,x,y,z| MATA120(v,x,y,z)}, 1, aCabec, aItens, 3 )

			If lMsErroAuto
				cMessage :='ERRO ROTINA AUTOMATICA CADASTRO DE PEDIDO DE COMPRAS'
                U_pFwLog("ERROR","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][ERROR] - '+cMessage)	

				aMsg := GetAutoGRLog()
				aEval(aMsg,{ |x| cErro += x + CRLF })
				oJsonRet['STATUS'] := .F.
                oJsonRet['WARN']   := U_pTrataErro(cErro)
				Return oJsonRet
			else
				oJsonRet['STATUS']           := .T.
                oJsonRet['PEDIDO_COMPRA'] := JsonObject():New()
                oJsonRet['PEDIDO_COMPRA'] := 'Inclusao de Pedido de Compras ['+SC7->C7_NUM+']'
				
                liberaPC(SC7->C7_NUM)

			EndIf

		END SEQUENCE

		ErrorBlock(bError)

		If (!Empty(cError))
			U_pFwLog("ERROR","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][ERROR] - '+Repl("-", 80))
			U_pFwLog("ERROR","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][ERROR] - '+PadC("Error: " + AllTrim(cError), 80))
			U_pFwLog("ERROR","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][ERROR] - '+Repl("-", 80))
		EndIf
	else
		U_pFwLog("ERROR","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][ERROR] - ARRAY DE OBJETOS - ITENS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO')
		oJsonRet['STATUS'] := .F.
		oJsonRet['WARN']   := " ARRAY DE OBJETOS - ITENS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO "
	endIf

	U_pFwLog("INFO","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][INFO] - FIM PROCESSO INCLUSAO DE PC')

Return oJsonRet
/*/{Protheus.doc} liberaPC

Funcao estatica para liberacao do pedido de compra

@type function
@author Katieli de Oliviera - BMTEC
@since 04/01/2022
@version P11,P12
@database SQL Server,Oracle

@history 04/01/2022, Liberacao inicial
@history 21/07/2023, Refatorado

@param cDoc, caracter, numero do pedido a ser liberado

@see PayTra05

/*/
Static Function liberaPC(cDoc)

	Local cAlias   := GetNextAlias()

	U_pFwLog("INFO","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][INFO] -  INICIO LIBERA  PC')
	
	//UPDATE SCR
	BeginSql Alias cAlias
        SELECT 
            R_E_C_N_O_ AS SCRREC
        FROM
            %Table:SCR% SCR
        WHERE 
                SCR.CR_FILIAL = %xFilial:SCR% 
            AND SCR.CR_NUM = %Exp:cDoc%
            AND SCR.CR_TIPO = %Exp:'IP'%
            AND SCR.%NotDel%
        
	EndSql
	(cAlias)->(DBGoTop())
	do While (cAlias)->( !Eof())
		dbSelectArea('SCR')
		SCR->(dbGoTo((cAlias)->SCRREC))
		SCR->(RecLock('SCR',.F.))
			SCR->CR_STATUS  := '03'
			SCR->CR_USERLIB := SCR->CR_USER
			SCR->CR_DATALIB := (DDATABASE)
			SCR->CR_OBS     := 'APROV AUTO PAYTRAC'
		SCR->(MsUnlock())
		(cAlias)->( DbSkip() )
	EndDo
	(cAlias)->(DbCloseArea())

	//UPDATE SC7
	cAlias   := GetNextAlias()
	BeginSql Alias cAlias
        SELECT 
            R_E_C_N_O_ AS SC7REC
        FROM
            %Table:SC7% SC7
        WHERE 
                SC7.C7_FILIAL = %xFilial:SC7% 
            AND SC7.C7_NUM = %Exp:cDoc%
            AND SC7.%NotDel%
        
	EndSql
	(cAlias)->(DBGoTop())
	Do While (cAlias)->( !Eof())
		dbSelectArea('SC7')
			SC7->(dbGoTo((cAlias)->SC7REC))
			SC7->(RecLock('SC7',.F.))
				SC7->C7_CONAPRO := 'L'
			SC7->(MsUnlock())
		(cAlias)->( DbSkip() )
	EndDo
	(cAlias)->(DbCloseArea())

	U_pFwLog("INFO","PayTra05",'[PAYTRACK][PEDIDOCOMPRA][INFO] - FIM LIBERA  PC')

Return
