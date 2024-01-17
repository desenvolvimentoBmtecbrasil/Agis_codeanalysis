#include "Totvs.ch"

/*/{Protheus.doc} PayTra03

Inclusao titulo

@type function
@author Katieli de Oliviera - BMTEC
@since 04/01/2022
@version P11,P12
@database SQL Server,Oracle

@history 04/01/2022, Liberacao inicial
@history 20/07/2023, Refatorado

@param oParseJSON, objeto, Objeto json com os dados enviados

@return caracter, Erro

@see WsPayTrac01
@see PayTra01
@see PayTra02
@see PayTra04
@see PayTra05
@see PayTra06

/*/

User Function PayTra03(oParse) 

    Local oJsonRet          := JsonObject():New()
	Local cAliasSE2         := GetNextAlias()
	Local aArray            := {}
	Local aAuxEv            := {}
	Local aAuxEz            := {}
	Local aRatEz            := {}
	Local aRatEvEz          := {}
	Local cErro             := ""
	Local aMsg              := {}
	Local _a                := 0
	Local _n                := 0
	Local _c                := 0
	Local x                 := 0
	Local cPre              := ""
	Local cNum              := ""
	Local cIdPay            := ""
	Local cPar              := ""
	Local cTip              := ""
	Local cFor              := ""
	Local cLoj              := ""
	Local cNatureza         := ""
	Local cError            := Space(0)
	Local bError            := ErrorBlock({|oError| cError := oError:Description})
	Local cMessage          := ''
    Local lError            := .F.
	Local cJson 			:= oParse:ToJson()
	Local oParseJSON 		:= Nil
	
	Private lMsErroAuto     := .F.
	Private lAutoErrNoFile  := .T.
	Private nFilial
	Private lOnline         := .t.
	Private aDadosCV4       := {}
	Private cSeqCJT         := ""
	Private cDebRat         := ""
	Private cHistRat        := ""
	Private aDRatCV4        := {}

    U_pFwLog("INFO","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][INFO] - INICIO ROTINA TITULO REEMBOLSO')
    
    oJsonRet['STATUS'] := JsonObject():New()
	oJsonRet['WARN']   := JsonObject():New()

	FWJsonDeserialize(DecodeUtf8(cJson),@oParseJSON)

	If !Empty(oParseJSON:TITULOS)

		dbSelectArea("SA2")
		dbSetOrder(3)

		For _a := 1 To len(oParseJSON:TITULOS)
			cAliasSE2         := GetNextAlias()
			//VALIDA ATRIBUTOS
            if     !AttIsMemberOf(oParseJSON:TITULOS[_a],"CGC")       	
                lError   := .T.
                cMessage := 'Atributo CGC nao informado no corpo da requisicao'
			elseIf !AttIsMemberOf(oParseJSON:TITULOS[_a],"BANCO")       	
                lError   := .T.
                cMessage := 'Atributo BANCO nao informado no corpo da requisicao'
			elseIf !AttIsMemberOf(oParseJSON:TITULOS[_a],"AGENCIA")       
                lError   := .T.
                cMessage := 'Atributo AGENCIA nao informado no corpo da requisicao'
			elseIf !AttIsMemberOf(oParseJSON:TITULOS[_a],"CONTA")       	
                lError   := .T.
                cMessage := 'Atributo CONTA nao informado no corpo da requisicao'
			elseIf !AttIsMemberOf(oParseJSON:TITULOS[_a],"PREFIXO")       
                lError   := .T.
                cMessage := 'Atributo PREFIXO nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"NUMERO")        
                lError   := .T.
                cMessage := 'Atributo NUMERO nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"PARCELA")       
                lError   := .T.
                cMessage := 'Atributo PARCELA nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"TIPO")          
                lError   := .T.
                cMessage := 'Atributo TIPO nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"NATUREZA")      
                lError   := .T.
                cMessage := 'Atributo NATUREZA nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"EMISSAO")       
                lError   := .T.
                cMessage := 'Atributo EMISSAO nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"VENCIMENTO")    
                lError   := .T.
                cMessage := 'Atributo VENCIMENTO nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"VALOR")         
                lError   := .T.
                cMessage := 'Atributo VALOR nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"HISTORICO")     
                lError   := .T.
                cMessage := 'Atributo HISTORICO nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"CONTACONTABIL") 
                lError   := .T.
                cMessage := 'Atributo CONTACONTABIL nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"CENTROCUSTO")   
                lError   := .T.
                cMessage := 'Atributo CENTROCUSTO nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"RATEIO")        
                lError   := .T.
                cMessage := 'Atributo RATEIO nao informado no corpo da requisicao'
            elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"MULTINAT")      
                lError   := .T.
                cMessage := 'Atributo MULTINAT nao informado no corpo da requisicao'
            endIf
            
            if lError
                oJsonRet['STATUS'] := .F.
                oJsonRet['WARN']   := cMessage
			    U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+cMessage)
			    return oJsonRet
            endIf

			If !SA2->( dbSeek( xFilial("SA2") + PadR( ALLTRIM(oParseJSON:TITULOS[_a]:CGC), TamSX3("A2_CGC")[01] ) ) )
				cMessage           := 'Fornecedor nao cadastrado [CGC]: '+PadR( ALLTRIM(oParseJSON:TITULOS[_a]:CGC), TamSX3("A2_CGC")[01] )
                oJsonRet['STATUS'] := .F.
                oJsonRet['WARN']   := cMessage
				U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+cMessage)
				return oJsonRet
			EndIf
             
			if alltrim(oParseJSON:TITULOS[_a]:TIPO)=='PA'
				If !SA6->( dbSeek( xFilial("SA6") + PadR( ALLTRIM(oParseJSON:TITULOS[_a]:BANCO), TamSX3("A6_COD")[01] )+ PadR( ALLTRIM(oParseJSON:TITULOS[_a]:AGENCIA), TamSX3("A6_AGENCIA")[01] )+ PadR( ALLTRIM(oParseJSON:TITULOS[_a]:CONTA), TamSX3("A6_NUMCON")[01] ) ) )
                    cMessage := 'Banco nao cadastrado [Banco]: '+PadR( ALLTRIM(oParseJSON:TITULOS[_a]:BANCO), TamSX3("A6_COD")[01] )+" - "+PadR( ALLTRIM(oParseJSON:TITULOS[_a]:AGENCIA), TamSX3("A6_AGENCIA")[01] )+" - "+PadR( ALLTRIM(oParseJSON:TITULOS[_a]:CONTA), TamSX3("A6_NUMCON")[01] )
                    oJsonRet['STATUS'] := .F.
                    oJsonRet['WARN']   := cMessage
				    U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+cMessage)
				    return oJsonRet
				endIf
			endIf

			cPre        := padr(alltrim(oParseJSON:TITULOS[_a]:PREFIXO),tamSx3('E2_PREFIXO')[1])
			cNum        := padr(alltrim(oParseJSON:TITULOS[_a]:NUMERO),tamSx3('E2_NUM')[1])
			cIdPay      := padr(alltrim(oParseJSON:TITULOS[_a]:NUMERO),tamSx3('E2_XPAYID')[1])
			cPar        := padr(alltrim(oParseJSON:TITULOS[_a]:PARCELA),tamSx3('E2_PARCELA')[1])
			cTip        := padr(alltrim(oParseJSON:TITULOS[_a]:TIPO),tamSx3('E2_TIPO')[1])
			cNatureza   := padr(alltrim(oParseJSON:TITULOS[_a]:NATUREZA),tamSx3('E2_NATUREZ')[1])
			
            cFor        := padr(alltrim(SA2->A2_COD),tamSx3('E2_FORNECE')[1])
			cLoj        := padr(alltrim(SA2->A2_LOJA),tamSx3('E2_LOJA')[1])

			BeginSql Alias cAliasSE2
			    SELECT
			        E2_XPAYID
			    FROM
			        %Table:SE2% SE2
			    WHERE
			            SE2.E2_FILIAL = %xFilial:SE2%
			        AND SE2.E2_XPAYID = %Exp:cIdPay%
			        AND SE2.E2_TIPO = %Exp:cValToChar(oParseJSON:TITULOS[_a]:TIPO)%
        			AND SE2.%NotDel%

			EndSql

			(cAliasSE2)->(DBGoTop())
			nTotal := 0
			(cAliasSE2)->(dbEval({||nTotal++},{||!DELETED()}))
			if nTotal == 0
				
				cMessage := GetLastQuery()[2]
				U_pFwLog("INFO","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][INFO] - '+cMessage)			

				aArray := { {"E2_FILIAL"  ,xFilial("SE2") ,NIL},;
							{"E2_PREFIXO" ,oParseJSON:TITULOS[_a]:PREFIXO ,NIL},;
                            {"E2_NUM"     ,oParseJSON:TITULOS[_a]:NUMERO ,NIL},;
                            {"E2_PARCELA" ,oParseJSON:TITULOS[_a]:PARCELA ,NIL},;
                            {"E2_TIPO"    ,oParseJSON:TITULOS[_a]:TIPO ,NIL},;
                            {"E2_NATUREZ" ,cNatureza,NIL},;
                            {"E2_FORNECE" ,SA2->A2_COD ,NIL},;
                            {"E2_LOJA"    ,SA2->A2_LOJA ,NIL},;
                            {"E2_EMISSAO" ,stod(oParseJSON:TITULOS[_a]:EMISSAO) ,NIL},;
                            {"E2_VENCTO " ,stod(oParseJSON:TITULOS[_a]:VENCIMENTO) ,NIL},;
                            {"E2_VALOR"   ,oParseJSON:TITULOS[_a]:VALOR,NIL},;
                            {"E2_HIST"    ,DecodeUTF8(oParseJSON:TITULOS[_a]:HISTORICO,'cp1252'),NIL},;
                            {"E2_CONTAD"  ,oParseJSON:TITULOS[_a]:CONTACONTABIL,NIL},;
                            {"E2_CCUSTO"  ,oParseJSON:TITULOS[_a]:CENTROCUSTO,NIL},;
                            {"E2_XPAYID"  ,oParseJSON:TITULOS[_a]:NUMERO,NIL},;
                            {"E2_CCD"     ,oParseJSON:TITULOS[_a]:CENTROCUSTO,NIL},;
                            {"E2_RATEIO"  ,oParseJSON:TITULOS[_a]:RATEIO,NIL},;
                            {"E2_MULTNAT" ,oParseJSON:TITULOS[_a]:MULTINAT,NIL}}
				ARatEvEz:={}
				If oParseJSON:TITULOS[_a]:MULTINAT == '1'
                    
                    if !AttIsMemberOf(oParseJSON:TITULOS[_a],"RATNAT")
                        cMessage := 'Atributo RATNAT nao informado no corpo da requisicao, Necessario informar quando atributo MULTINAT for igual 1'
                        oJsonRet['STATUS'] := .F.
                        oJsonRet['WARN']   := cMessage
		        	    U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+cMessage)
        			    return oJsonRet
                    endIf

					for _n:=1 to len(oParseJSON:TITULOS[_a]:RATNAT)
						
                        aAuxEv := {}
						aRatEz := {}

                        if !AttIsMemberOf(oParseJSON:TITULOS[_a]:RATNAT[_n],"NATUREZA")
                            lError   := .T.
                            cMessage := 'Atributo NATUREZA dentro do array de objetos RATNAT nao foi informado no corpo da requisicao'
                        elseif !AttIsMemberOf(oParseJSON:TITULOS[_a]:RATNAT[_n],"VALORRAT")
                            lError   := .T.
                            cMessage := 'Atributo VALORRAT dentro do array de objetos RATNAT nao foi informado no corpo da requisicao'
                        elseif !AttIsMemberOf(oParseJSON:TITULOS[_a]:RATNAT[_n],"PERRAT")  
                            lError   := .T.
                            cMessage := 'Atributo PERRAT dentro do array de objetos RATNAT nao foi informado no corpo da requisicao'
                        elseif !AttIsMemberOf(oParseJSON:TITULOS[_a]:RATNAT[_n],"RATCC")   
                            lError   := .T.
                            cMessage := 'Atributo RATCC dentro do array de objetos RATNAT nao foi informado no corpo da requisicao'
                        endIf

                        if lError
                            oJsonRet['STATUS'] := .F.
                            oJsonRet['WARN']   := cMessage
			                U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+cMessage)
			                return oJsonRet
                        endIf

						//Adicionando o vetor da natureza
						aadd( aAuxEv ,{"EV_NATUREZ" , oParseJSON:TITULOS[_a]:RATNAT[_n]:NATUREZA,           Nil })//natureza a ser rateada
						aadd( aAuxEv ,{"EV_VALOR"   , oParseJSON:TITULOS[_a]:RATNAT[_n]:VALORRAT,           Nil })//valor do rateio na natureza
						aadd( aAuxEv ,{"EV_PERC"    , oParseJSON:TITULOS[_a]:RATNAT[_n]:PERRAT,             Nil })//percentual do rateio na natureza
						aadd( aAuxEv ,{"EV_RATEICC" , CVALTOCHAR(oParseJSON:TITULOS[_a]:RATNAT[_n]:RATCC),  Nil })//indicando que há rateio por centro de custo

						//primeiro centro de custo
						if oParseJSON:TITULOS[_a]:RATNAT[_n]:RATCC == '1'
							for _c:=1 to len(oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC)
                            
                                if !AttIsMemberOf(oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c],"CC")
                                    lError   := .T.
                                    cMessage := 'Atributo CC dentro do array de objetos ARATCC nao foi informado no corpo da requisicao'
                                elseif !AttIsMemberOf(oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c],"CCITEM") 
                                    lError   := .T.
                                    cMessage := 'Atributo CCITEM dentro do array de objetos ARATCC nao foi informado no corpo da requisicao'
                                elseif !AttIsMemberOf(oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c],"DEBITO") 
                                    lError   := .T.
                                    cMessage := 'Atributo DEBITO dentro do array de objetos ARATCC nao foi informado no corpo da requisicao'
                                elseif !AttIsMemberOf(oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c],"CLVLDB") 
                                    lError   := .T.
                                    cMessage := 'Atributo CLVLDB dentro do array de objetos ARATCC nao foi informado no corpo da requisicao'
                                elseif !AttIsMemberOf(oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c],"VALORRAT")
                                    lError   := .T.
                                    cMessage := 'Atributo VALORRAT dentro do array de objetos ARATCC nao foi informado no corpo da requisicao'
                                elseif !AttIsMemberOf(oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c],"PERRAT")  
                                    lError   := .T.
                                    cMessage := 'Atributo PERRAT dentro do array de objetos ARATCC nao foi informado no corpo da requisicao'
                                endIf

                                if lError
                                    oJsonRet['STATUS'] := .F.
                                    oJsonRet['WARN']   := cMessage
                                    U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+cMessage)
                                    return oJsonRet
                                endIf

								aAuxEz:={}
								aadd( aAuxEz ,{"EZ_CCUSTO" ,  oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c]:CC, Nil       })//centro de custo da natureza
								aadd( aAuxEz ,{"EZ_ITEMCTA",  oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c]:CCITEM, Nil   })//centro de custo da natureza
								aadd( aAuxEz ,{"EZ_CONTA",    oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c]:DEBITO, Nil   })//centro de custo da natureza
								aadd( aAuxEz ,{"EZ_CLVL",     oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c]:CLVLDB, Nil   })//centro de custo da natureza
								aadd( aAuxEz ,{"EZ_VALOR",    oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c]:VALORRAT, Nil })//valor do rateio neste centro de custo
								aadd( aAuxEz ,{"EZ_PERC",     oParseJSON:TITULOS[_a]:RATNAT[_n]:ARATCC[_c]:PERRAT, Nil   })//valor do rateio neste centro de custo
								aadd(aRatEz,aAuxEz)
								
							NEXT _c
						EndIf

						aadd(aAuxEv,{"AUTRATEICC" , aRatEz, Nil })//recebendo dentro do array da natureza os multiplos centros de custo
						aAdd(aRatEvEz,aAuxEv)//adicionando a natureza ao rateio de multiplas naturezas

					Next _n
					aAdd(aArray,{"AUTRATEEV",ARatEvEz,Nil})//adicionando ao vetor aCab o vetor do rateio
				Else
					if oParseJSON:TITULOS[_a]:NATUREZA <> ''
						aAdd(aArray,{ "E2_NATUREZ" , oParseJSON:TITULOS[_a]:NATUREZA, NIL })
					EndIf
				EndIf

				if Alltrim(oParseJSON:TITULOS[_a]:TIPO) == 'PA'
                    if !AttIsMemberOf(oParseJSON:TITULOS[_a],"BANCO")      
                        lError   := .T.
                        cMessage := 'Atributo BANCO nao informado no corpo da requisicao'
                    elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"AGENCIA")    
                        lError   := .T.
                        cMessage := 'Atributo AGENCIA nao informado no corpo da requisicao'
                    elseif !AttIsMemberOf(oParseJSON:TITULOS[_a],"CONTA")      
                        lError   := .T.
					    cMessage := 'Atributo CONTA nao informado no corpo da requisicao'
                    endIf

                    if lError
                        oJsonRet['STATUS'] := .F.
                        oJsonRet['WARN']   := cMessage
			            U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+cMessage)
			            return oJsonRet
                    endIf
					
                    U_pFwLog("INFO","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][INFO] - BANCO')
					
					aAdd(aArray,{ "AUTBANCO"   , oParseJSON:TITULOS[_a]:BANCO, NIL })
					aAdd(aArray,{ "AUTAGENCIA" , oParseJSON:TITULOS[_a]:AGENCIA, NIL })
					aAdd(aArray,{ "AUTCONTA"   , oParseJSON:TITULOS[_a]:CONTA, NIL })

				endIf

				if !Empty(aArray)

					if oParseJSON:TITULOS[_a]:RATEIO =='S'

						aDadosCV4 := oParseJSON:TITULOS[_a]:ARATEIO
						cSeqCJT :=  GetSxeNum("CTJ","CTJ_RATEIO")

						DbSelectArea("CTJ")
						For x:= 1 to len(aDadosCV4)

							RecLock("CTJ", .T.)
							CTJ->CTJ_FILIAL := cFilAnt
							CTJ->CTJ_RATEIO := cSeqCJT
							CTJ->CTJ_MOEDLC := "01"
							CTJ->CTJ_TPSALD := "1"
							CTJ->CTJ_SEQUEN  := STRZERO(x, 3)
							CTJ->CTJ_DESC := "PAYTRACK"
							CTJ->CTJ_DEBITO := aDadosCV4[x]:DEBITO
							CTJ->CTJ_PERCEN := val(aDadosCV4[x]:PERRAT)
							CTJ->CTJ_HIST := DecodeUTF8(aDadosCV4[x]:HISTORICO,'cp1252')
							CTJ->CTJ_CCD := aDadosCV4[x]:CC
							CTJ->CTJ_ITEMD := aDadosCV4[x]:ITEMCTB
							CTJ->CTJ_CLVLDB := aDadosCV4[x]:CLVLDB

							MsUnLock()

							aadd(aDRatCV4, {cSeqCJT, aDadosCV4[x]:DEBITO, aDadosCV4[x]:HISTORICO})

						Next x

					EndIf

					lMsErroAuto    := .F.
					lAutoErrNoFile := .T.

					BEGIN SEQUENCE

						MSExecAuto({|x,y| FINA050(x,y)}, aArray, 3)

						if lMsErroAuto

							cMessage :='ERRO ROTINA AUTOMATICA CADASTRO DE TITULO REEMBOLSO'
                            U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+cMessage)	

							aMsg := GetAutoGRLog()
							aEval(aMsg,{ |x| cErro += x + CRLF })
							oJsonRet['STATUS'] := .F.
                            oJsonRet['WARN']   := U_pTrataErro(cErro)
							return oJsonRet
							
						else

	                        oJsonRet['STATUS']           := .T.
                            oJsonRet['TITULO_REEMBOLSO'] := JsonObject():New()
                            oJsonRet['TITULO_REEMBOLSO'] := 'Inclusao de Titulo de Reembolso'
							
						endIf

					END SEQUENCE

					ErrorBlock(bError)

					If (!Empty(cError))
                        U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+Repl("-", 80))
                        U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+PadC("Error: " + AllTrim(cError), 80))
                        U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+Repl("-", 80))
					EndIf

				endIf
				
				(cAliasSE2)->(DbCloseArea())
			
			else

                oJsonRet['STATUS']  := .F.
                oJsonRet['WARN']    := "Duplicidade de Registro. Titulo ja cadastrado."
				cMessage := 'TITULO REEMBOLSO JA CADASTRADO'
                U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+cMessage)
				exit

			endIf

			
		Next a
	else
		U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - ARRAY DE OBJETOS - TITULOS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO')
        oJsonRet['STATUS'] := .F.
        oJsonRet['WARN']   := " ARRAY DE OBJETOS - TITULOS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO "
	endIf

    U_pFwLog("INFO","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][INFO] - FIM  ROTINA TITULO REEMBOLSO')
	
Return oJsonRet



