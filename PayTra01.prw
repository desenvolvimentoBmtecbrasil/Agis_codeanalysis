#include "Totvs.ch"

/*/{Protheus.doc} PayTra01

Rotina para Validar se ID paytrack ja incluido

@type function
@author Katieli de Oliviera - BMTEC
@since 04/01/2022
@version P11,P12
@database SQL Server,Oracle

@history 04/01/2022, Liberacao inicial
@history 20/07/2023. Refatorado

@param oParseJSON, objeto, Objeto json com os dados enviados
@param lConta, Logico, deve incrementar a numeração 

@return caracter, ID valido

@see WsPayTrac01
@see PayTra02
@see PayTra03
@see PayTra04
@see PayTra05
@see PayTra06

/*/

User Function PayTra01(oParse)

	Local cAliasSE2 	:= GetNextAlias()
	Local cID       	:= ""
	Local cJson 			:= oParse:ToJson()
	Local oParseJSON 	:= Nil
	Local oJsonRet      := JsonObject():New()
	Local lError        := .F.
	Local nTotal        := 0

	U_pFwLog("INFO","PayTra01",'[PayTrack][TITULO_DISPONIVEL][INFO] - INICIO TITULO_DISPONIVEL')

	oJsonRet['STATUS'] := JsonObject():New()
	oJsonRet['WARN']   := JsonObject():New()

	FWJsonDeserialize(DecodeUtf8(cJson),@oParseJSON)

	if	!AttIsMemberOf(oParseJSON,"ID")
		lError   := .T.
		cMessage := 'Atributo ID nao informado no corpo da requisicao'
	endIf

	if lError
		oJsonRet['STATUS'] := .F.
		oJsonRet['WARN']   := cMessage
		U_pFwLog("ERROR","PayTra01",'[PAYTRACK][TITULO_DISPONIVEL][ERROR] - '+cMessage)
		return oJsonRet
	endIf

	BeginSql Alias cAliasSE2
		 
	SELECT 
		E2_XPAYID
	FROM
		%Table:SE2% SE2
	WHERE 
		    SE2.E2_FILIAL = %xFilial:SE2% 
        AND SE2.E2_XPAYID = %Exp:cID%
        AND SE2.%NotDel%
	
	EndSql

	(cAliasSE2)->(DBGoTop())
	(cAliasSE2)->(dbEval({||nTotal++},{||!DELETED()}))

	If nTotal == 0
		U_pFwLog("INFO","PayTra01",'[PayTrack][TITULO_DISPONIVEL][INFO] - TITULO DISPONIVEL')
		oJsonRet['STATUS'] := .T.
		oJsonRet['VALIDAID'] := "Id disponivel"
	else
		U_pFwLog("WARN","PayTra01",'[PayTrack][TITULO_DISPONIVEL][WARN] - TITULO DISPONIVEL')
		oJsonRet['STATUS'] := .F.
		oJsonRet['WARN'] := 'Id ja incluido'
		(cAliasSE2)->(DbCloseArea())
	endIf
	
	U_pFwLog("INFO","PayTra01",'[PayTrack][TITULO_DISPONIVEL][INFO] - FIM TITULO_DISPONIVEL')

Return oJsonRet
