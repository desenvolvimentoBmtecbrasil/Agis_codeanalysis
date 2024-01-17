#include "Totvs.ch"

/*/{Protheus.doc} PayTra10

Busca dados Centros Custo

@type function
@author Katieli de Oliviera - BMTEC
@since 16/02/2022
@version P11,P12
@database SQL Server,Oracle

@history 16/02/2022, Liberacao inicial
@history 21/07/2022, Refatorado

@param oParseJSON, objeto, Objeto json com os dados enviados

@return caracter, Erro

@see WsPayTrac01
@see PayTra01
@see PayTra02
@see PayTra04
@see PayTra05
@see PayTra06

/*/

User Function PayTra10(oParse)

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

	U_pFwLog("INFO","PayTra10",'[PAYTRACK][CENTROCUSTO][INFO] - INICIO ROTINA CENTROS CUSTO')

	oJsonRet['STATUS'] := JsonObject():New()
	oJsonRet['WARN']   := JsonObject():New()

	FWJsonDeserialize(DecodeUtf8(cJson),@oParseJSON)

	If (oParseJSON['TIPO']) == 0 .OR. ( oParseJSON['TIPO'] <> 0 .AND. !Empty(oParseJSON['DATAINI']) .AND. !Empty(oParseJSON['DATAINI']) )

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

		U_pFwLog("INFO","PayTra10",'[PAYTRACK][CENTROCUSTO][INFO] - '+cMessage)

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
		
		(cAliasQry)->(DbCloseArea())
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

	U_pFwLog("INFO","PayTra10",'[PAYTRACK][CENTROCUSTO][INFO] - FIM ROTINA CENTROS CUSTO')
	

Return oJsonRet
