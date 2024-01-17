#include "Totvs.ch"
#include "protheus.ch"
#include "rptdef.ch"
#include "fwmvcdef.ch"
#include "tbiconn.ch"
#include "rwmake.ch"
#include "topconn.ch"
#Include 'RestFul.CH'
#Include "ApWebSrv.ch"
#include "TbiCode.ch"

/*/{Protheus.doc} PayTra10

Busca dados Aprovadores
@type function
@author Katieli de Oliviera - BMTEC
@since 16/02/2022
@version P11,P12
@database SQL Server,Oracle

@history 16/02/2022, Liberacao inicial

@param oParseJSON, objeto, Objeto json com os dados enviados

@return caracter, Erro

@see WsPayTrac01
@see PayTra01
@see PayTra02
@see PayTra04
@see PayTra05
@see PayTra06

/*/

User Function PayTra13(oParse)
	
	Local oJsonRet  	:= JsonObject():New()
	Local cAliasQry 	:= GetNextAlias()
	Local aCC       	:= {}
	Local cMessage  	:= ''
	Local cWhere    	:= ''
	Local nX        	:= 0
	Local lRet      	:= .T.
	Local cJson 		:= oParse:ToJson()
	Local oParseJSON 	:= Nil

	Private lMsErroAuto    := .F.
	Private lAutoErrNoFile := .T.


	U_pFwLog("INFO","PayTra13",'[PAYTRACK][APROVADORES][INFO] - INICIO ROTINA DE APROVADORES')

	oJsonRet['STATUS'] := JsonObject():New()
	oJsonRet['WARN']   := JsonObject():New()
	
	FWJsonDeserialize(DecodeUtf8(cJson),@oParseJSON)
	
	if     !AttIsMemberOf(oParseJSON,"TIPO")
		lError   := .T.
		cMessage := 'Atributo TIPO nao informado no corpo da requisicao'
	elseif !AttIsMemberOf(oParseJSON,"DATAINI")
		lError   := .T.
		cMessage := 'Atributo DATAINI nao informado no corpo da requisicao'
	endif

	if lError
		oJsonRet['STATUS'] := .F.
		oJsonRet['WARN']   := cMessage
		U_pFwLog("ERROR","PayTra13",'[PAYTRACK][APROVADORES][ERROR] - '+cMessage)
		return oJsonRet
	endIf



	If (oParseJSON['TIPO'])==0 .OR. ( oParseJSON['TIPO']<>0 .AND. !Empty(oParseJSON['DATAINI']) .AND. !Empty(oParseJSON['DATAINI']) )

		IF oParseJSON['TIPO'] <> 0
			cWhere := "%"
			cWhere += "CAST(TO_CHAR(CTT.S_T_A_M_P_,'YYYYMMDD') AS varchar2(27) ) BETWEEN '" + oParseJSON['DATAINI'] + "' AND '" + oParseJSON['DATAFIM'] + "'"
			cWhere += "%"
		else
			cWhere	:= "%  1=1 %"
		EndIf

		BeginSql Alias cAliasQry
                SELECT 
                    CTT_CUSTO, 
                    CTT_CCSUP, 
                    CTT_DESC01, 
                    CTT_BLOQ,
                    (
                        SELECT 
                            CTT_DESC01 
                        FROM 
                           %Table:CTT% CT 
                        WHERE 
                            rownum = 1 and 
                            CT.CTT_CUSTO = CTT.CTT_CCSUP
                    ) AS DSC_CCSUP 

                FROM 
                    %Table:CTT% CTT
                WHERE 
                        CTT.CTT_FILIAL = %xFilial:CTT%
                    AND CTT.CTT_BLOQ <> %exp:'1'%  
                    AND CTT.%NotDel% 
                	AND %Exp:cWhere%
                ORDER BY 
                    R_E_C_N_O_ DESC 
		endSql

		cMessage := GetLastQuery()[2]

		U_pFwLog("INFO","PayTra13",'[PAYTRACK][APROVADORES][INFO] - '+cMessage)

		(cAliasQry)->(DBGoTop())

		oJsonRet['CENTROS_CUSTO'] := JsonObject():New()

		do While (cAliasQry)->(!EoF())

			lRet:=.T.
			nX++
			aadd(aCC,JsonObject():new())

			aCC[nX]["CODIGO"]     := ALLTRIM( (cAliasQry)->CTT_CUSTO )
			aCC[nX]["DESCRI"]     := ALLTRIM( (cAliasQry)->CTT_DESC01 )
			aCC[nX]["COD_PAI"]    := ALLTRIM( (cAliasQry)->CTT_CCSUP )
			aCC[nX]["DESCRI_PAI"] := ALLTRIM( (cAliasQry)->DSC_CCSUP )
			aCC[nX]["STATUS"]     := ALLTRIM( (cAliasQry)->CTT_BLOQ )

			(cAliasQry)->(dbSkip())

		EndDo

		oJsonRet['CENTROS_CUSTO'] := aCC

		U_pFwLog("INFO","PayTra10",'[PAYTRACK][CENTROCUSTO][INFO] - '+oJsonRet:ToJson())

		if lRet
			oJsonRet['STATUS'] := .T.
			oJsonRet['WARN'] := "Dados encontrados e retornados"
		else
			oJsonRet['STATUS'] := .F.
			oJsonRet['WARN'] := "Dados nao encontrados e retornados"
		endIf

	else
		oJsonRet['WARN']   := "DATAS OU TIPO NAO INFORMADOS NOS PARAMETROS"
		oJsonRet['STATUS'] := .F.
	endIf

	U_pFwLog("INFO","PayTra13",'[PAYTRACK][APROVADORES][INFO] - FIM ROTINA DE APROVADORES')

Return oJsonRet
