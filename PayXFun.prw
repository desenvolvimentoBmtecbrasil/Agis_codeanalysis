#include "Totvs.ch"

User Function pTrataErro(cErroAuto)

	Local nLines   := MLCount(cErroAuto)
	Local cNewErro := ""
	Local nErr        := 0

	For nErr := 1 To nLines
		cNewErro += AllTrim( MemoLine( cErroAuto, , nErr ) ) + " - "
	Next nErr

Return(cNewErro)

User Function pFwLog(cSeverity,cGroup,cMessage)

	default cSeverity := "ERROR"
	default cGroup    := "PayTrack"
	default cMessage  := "VAZIO"

	FWLogMsg(cSeverity, /*cTransactionId*/, cGroup, /*cCategory*/, /*cStep*/, /*cMsgId*/, cMessage, /*nMensure*/, /*nElapseTime*/, /*aMessage*/)

Return

User Function isInTrans()
	Local lRet := .F.
	If Intransaction()
		lRet := .T.
	EndIf
Return lRet

//Mota retorno de erro - Json
User Function montaErro(cMensagem, nHTTPCod, nCodWS)

	oReturn["resultado"] := "ERRO"
	oReturn["mensagem"] := encodeUTF8(cMensagem + ' | Tempo gasto: ' + Tgasto())
	oReturn["numeroPreNota"] := ""

	nStatusCode := nHTTPCod
	nCodErro    := nCodWS
	lOK := .F.

Return
//Console log
User Function logWS(cMsgLog)

	Local cDtHr := dtos(date()) + ' ' + time()

	conout('[preNota] ' + cDtHr + ' : ' + cMsgLog)

Return
//Retorna tempo de processamento da acao executada
User Function Tgasto()

	Local cDiff := ''
	Local cElapsed := ''

	cDiff := cvaltochar(timecounter() - nHrIni)
	cElapsed := substr(cDiff,1,at('.',cDiff)+3) + 'ms'

return cElapsed
//Testa se TenantID foi passado e se esta no formato correto
User Function trataAmbiente(cTenantId)

	Local cEmpK := ''
	Local cFilK := ''

	u_logWS('trataAmbiente()')

	cEmpK := alltrim(substr(cTenantId,1,at(',',cTenantId)-1))
	cFilK := alltrim(substr(cTenantId,at(',',cTenantId)+1))

	Do Case
	Case empty(cEmpK)
		u_montaErro("Header tenantId enviado no formato incorreto", 400, 2)
	Case empty(cFilK)
		u_montaErro("Header tenantId enviado no formato incorreto", 400, 3)
	Case len(cEmpK) <> len(alltrim(cEmpAnt))
		u_montaErro("Header tenantId enviado no formato incorreto", 400, 4)
	Case len(cFilK) <> len(alltrim(cFilAnt))
		u_montaErro("Header tenantId enviado no formato incorreto", 400, 5)
	EndCase


Return

User Function getMsgErro(aErro)
	Local cMsg   := ""
	Local nCount := 0
	
	For nCount := 1 To Len(aErro)
		If AT(':=',aErro[nCount]) > 0 .And. AT('< --',aErro[nCount]) < 1
			Loop
		EndIf
		If AT("------", aErro[nCount]) > 0
			Loop
		EndIf
		If !Empty(cMsg)
			cMsg += " "
		EndIf
		cMsg += AllTrim(StrTran( StrTran( StrTran( StrTran( StrTran( aErro[nCount], "/", "" ), "<", "" ), ">", "" ), CHR(10), " "), CHR(13), "") + ("|"))
	Next nCount
	
	If Empty(cMsg) .And. Len(aErro) > 0
		For nCount := 1 To Len(aErro)
			If !Empty(cMsg)
				cMsg += " "
			EndIf
			cMsg += AllTrim(StrTran( StrTran( StrTran( StrTran( StrTran( aErro[nCount], "/", "" ), "<", "" ), ">", "" ), CHR(10), " "), CHR(13), "") + ("|"))
		Next nCount
	EndIf

Return cMsg
