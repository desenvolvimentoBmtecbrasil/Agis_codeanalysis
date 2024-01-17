#include "Totvs.ch"

/*/{Protheus.doc} PayTra11

Compensacao titulo

@type function
@author Katieli de Oliviera - BMTEC
@since 01/03/2022
@version P11,P12
@database SQL Server,Oracle

@history 01/03/2022, Liberacao inicial
@history 01/03/2022, Refatorado

@param oParseJSON, objeto, Objeto json com os dados enviados

@return caracter, Erro

@see WsPayTrac01
@see PayTra01
@see PayTra02
@see PayTra04
@see PayTra05
@see PayTra06

/*/

User Function PayTra11(oParse) 

	local aPerg as array
	local aRet as array
	local aEstorno as array
	local aRecTit as array
	local aRecCmp as array
	local aRecEst as array
	local cRecTit as character
	local cMessage as character
	local cRecCmp as character
	local cJanela as character
	local cCodCanc as character
	local cValor as character
	local lContabiliza as numeric
	local lAglutina as numeric
	local lDigita as numeric
	local lHelp as logical
	local nSldComp as numeric
	local nHdl as numeric
	local nOperacao as numeric
	local _a as numeric
    
    Local oJsonRet    := JsonObject():New()
    Local lError      := .F.

	Local cJson 			:= oParse:ToJson()
	Local oParseJSON 		:= Nil

	Local cAliasQry    := GetNextAlias()  
	
    Private CODFORCP  := ''
	Private LOJFORCP  := ''

	cJanela     := "Compensacao automatica"
	aPerg       := {}
	aRet        := {}
	aEstorno    := {}
	aRecTit     := {}
	aRecCmp     := {}
	aRecEst     := {}
	cValor      := cCodCanc := cRecTit := cRecCmp := replicate(" ",50)
	nSldComp    := 0
	nVlPA       := 0
	nRecPA      := 0
	nVlRDP      := 0
	nRecRDP     := 0
	nHdl        := 0
    _a          := 0
	nOperacao   := 0
	lHelp       := .F.
	cMessage    := ''

	U_pFwLog("INFO","PayTra11",'[PAYTRACK][COMPENSATITULO][INFO] - INICIO ROTINA COMPENSA TITULO')

    oJsonRet['STATUS'] := JsonObject():New()
	oJsonRet['WARN']   := JsonObject():New()
	
	FWJsonDeserialize(DecodeUtf8(cJson),@oParseJSON)
	
	If !Empty(oParseJSON:Titulos)

		lContabiliza    := 1
		lAglutina       := 1
		lDigita         := 1
		
        For _a := 1 To len(oParseJSON:Titulos)

            if  !AttIsMemberOf(oParseJSON:Titulos[_a],"ID_PA") 
                lError   := .T.
                cMessage := 'Atributo ID_PA nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"ID_COMP") 
                lError   := .T.
                cMessage := 'Atributo ID_COMP nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"VALOR") 
                lError   := .T.
                cMessage := 'Atributo VALOR nao informado no corpo da requisicao'
            endIf

              if lError
                oJsonRet['STATUS'] := .F.
                oJsonRet['WARN']   := cMessage
			    U_pFwLog("ERROR","PayTra11",'[PAYTRACK][COMPENSATITULO][ERROR] - '+cMessage)
			    return oJsonRet
            endIf

            BeginSql Alias cAliasQry

			    SELECT 
			        R_E_C_N_O_ AS REC, 
                    E2_XPAYID, 
                    E2_PREFIXO, 
                    E2_NUM, 
                    E2_PARCELA, 
                    E2_TIPO, 
                    E2_FORNECE,
                    E2_LOJA, 
                    E2_VALOR, 
                    E2_EMISSAO, 
                    E2_VENCTO, 
                    E2_VENCREA, 
                    E2_BAIXA, 
			        CASE  WHEN E2_TIPO = 'PA' THEN 1 ELSE 2 END TIPO 
				FROM 
                    %Table:SE2% SE2
			    WHERE 
                        SE2.E2_FILIAL = %xFilial:SE2%
                    AND SE2.%NotDel% 
			        AND 
                        (
                                (       
                                        E2_XPAYID  = %Exp:cValToChar(oParseJSON:Titulos[_a]:ID_PA)%
                                    AND E2_TIPO= %Exp:'PA'%
                                ) 
                            OR 
                                ( 
                                    E2_XPAYID  = %Exp:cValToChar(oParseJSON:Titulos[_a]:ID_COMP)%
                                )
                        )
			    ORDER BY 
                    SE2.R_E_C_N_O_ DESC

            endSql

			cMessage := GetLastQuery()[2]
            U_pFwLog("INFO","PayTra11",'[PAYTRACK][COMPENSATITULO][INFO] - '+cMessage)			
			
			lRet:= .T.

            (cAliasQry)->(DBGoTop())

			While (cAliasQry)->(!EoF())
				CODFORCP  := (cAliasQry)->E2_FORNECE
				LOJFORCP  := (cAliasQry)->E2_LOJA

				if ALLTRIM((cAliasQry)->E2_TIPO)=="PA"

					aAdd(aRecTit, ((cAliasQry)->REC))
					nVlPA := (cAliasQry)->E2_VALOR
					nRecPA := (cAliasQry)->REC
				else

					aAdd(aRecCmp, ((cAliasQry)->REC))
					nVlRDP := (cAliasQry)->E2_VALOR
					nRecRDP := (cAliasQry)->REC
				EndIf
				(cAliasQry)->(dbSkip())
			EndDo
			
            nSldComp := iif(oParseJSON:Titulos[_a]:VALOR >nVlPA , nVlPA, oParseJSON:Titulos[_a]:VALOR)
			
			cMessage := 'Valor a COMPENSAR : '+cvaltochar(nSldComp)
			U_pFwLog("INFO","PayTra11",'[PAYTRACK][COMPENSATITULO][INFO] - '+cMessage)

			if !FinCmpAut(aRecTit, aRecCmp, {lContabiliza,lAglutina,lDigita}, /*bBlock*/, aEstorno, nSldComp, dDatabase, /*nTaxaPA*/,/*nTaxaNF*/, nHdl, nOperacao, /*aRecSE5*/, /*aNDFDados*/, lHelp)
				cMessage :='Titulo nao COMPENSADO [ID]: '+PadR( ALLTRIM(oParseJSON:Titulos[_a]:ID_PA), TamSX3("E2_XPAYID")[01] )
                oJsonRet['STATUS'] := .F.
                oJsonRet['WARN']   := cMessage
			    U_pFwLog("ERROR","PayTra11",'[PAYTRACK][COMPENSATITULO][ERROR] - '+cMessage)
			    
			else
				cMessage :='Titulo  COMPENSADO [ID]: '+PadR( ALLTRIM(oParseJSON:Titulos[_a]:ID_PA), TamSX3("E2_XPAYID")[01] )
				oJsonRet['STATUS']           := .T.
                oJsonRet['COMPENSA_TITULO'] := JsonObject():New()
                oJsonRet['COMPENSA_TITULO'] := cMessage
				if oParseJSON:Titulos[_a]:VALOR > nVlPA
					DbSelectArea('SE2')
					DbGoTo(nRecPA)
					If SE2->(Recno()) == nRecPA
						If  SE2->E2_SALDO <> 0
							RecLock('SE2', .F.)
							    SE2->E2_SALDO :=0
							MsUnLock()
						EndIf
					EndIf

					DbSelectArea('SE2')
					DbGoTo(nRecRDP)
					If SE2->(Recno()) == nRecRDP
						If  SE2->E2_SALDO == 0
							RecLock('SE2', .F.)
							    SE2->E2_SALDO := nVlRDP-nVlPA
							MsUnLock()
						EndIf
					EndIf
				endif
			EndIf

		Next _a
	else
		U_pFwLog("ERROR","PayTra11",'[PAYTRACK][COMPENSATITULO][ERROR] - ARRAY DE OBJETOS - TITULOS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO')
        oJsonRet['STATUS'] := .F.
        oJsonRet['WARN']   := " ARRAY DE OBJETOS - TITULOS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO "
	endIf
    
    U_pFwLog("INFO","PayTra11",'[PAYTRACK][COMPENSATITULO][INFO] - INICIO ROTINA COMPENSA TITULO')

Return oJsonRet
