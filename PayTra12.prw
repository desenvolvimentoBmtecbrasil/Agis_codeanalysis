#include "Totvs.ch"

/*/{Protheus.doc} PayTra12

Inclusao titulo

@type function
@author Katieli de Oliviera - BMTEC
@since 30/01/2023
@version P11,P12
@database SQL Server,Oracle

@history 30/01/2023, Liberacao inicial
@history 21/07/2023, Refatorado

@param oParseJSON, objeto, Objeto json com os dados enviados

@return caracter, Erro

@see WsPayTrac01
@see PayTra01
@see PayTra02
@see PayTra04
@see PayTra05
@see PayTra06

/*/

User Function PayTra12(oParse)

	Local oJsonRet          := JsonObject():New()
	Local cAliasPay         := GetNextAlias()
	Local aArray            := {}
	Local aAuxEv            := {}
	Local aAuxEz            := {}
	Local aRatEz            := {}
	Local aRatEvEz          := {}
	Local aMsg              := {}
	Local _n                := 0
	Local _c                := 0
	Local x                 := 0
	Local aVetSE1
	Local cErro             := ''
	Local cIdPay            := ''
	local _a                := 1
	local nRecno            := 0
	Local cError 			:= Space(0)
	Local bError 			:= ErrorBlock({|oError| cError := oError:Description})
	Local cMessage  		:= ''

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

	U_pFwLog("INFO","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][INFO] - INICIO ROTINA TITULO DEVOLUCAO')

	oJsonRet['STATUS'] := JsonObject():New()
	oJsonRet['WARN']   := JsonObject():New()

	FWJsonDeserialize(DecodeUtf8(cJson),@oParseJSON)

	If !Empty(oParseJSON:Titulos)

		dbSelectArea("SA1")
		dbSetOrder(3)

		For _a := 1 To len(oParseJSON:Titulos)

			If !SA1->( dbSeek( xFilial("SA1") + PadR( ALLTRIM(oParseJSON:Titulos[_a]:CGC), TamSX3("A1_CGC")[01] ) ) )
				cMessage           := 'Fornecedor nao cadastrado [CGC]: '+PadR( ALLTRIM(oParseJSON:Titulos[_a]:CGC), TamSX3("A2_CGC")[01] )
				oJsonRet['STATUS'] := .F.
				oJsonRet['WARN']   := cMessage
				U_pFwLog("ERROR","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][ERROR] - '+cMessage)
				return oJsonRet
			EndIf

			//VALIDA ATRIBUTOS
			if     !AttIsMemberOf(oParseJSON:Titulos[_a],"PREFIXO")
				lError   := .T.
				cMessage := 'Atributo PREFIXO nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"NUMERO")
				lError   := .T.
				cMessage := 'Atributo NUMERO nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"PARCELA")
				lError   := .T.
				cMessage := 'Atributo PARCELA nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"TIPO")
				lError   := .T.
				cMessage := 'Atributo TIPO nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"NATUREZA")
				lError   := .T.
				cMessage := 'Atributo NATUREZA nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"EMISSAO")
				lError   := .T.
				cMessage := 'Atributo EMISSAO nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"VENCTO")
				lError   := .T.
				cMessage := 'Atributo VENCTO nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"VALOR")
				lError   := .T.
				cMessage := 'Atributo VALOR nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"HISTORICO")
				lError   := .T.
				cMessage := 'Atributo HISTORICO nao informado no corpo da requisicao'
			elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"MULTINAT")
				lError   := .T.
				cMessage := 'Atributo MULTINAT nao informado no corpo da requisicao'
			endIf

			if lError
				oJsonRet['STATUS'] := .F.
				oJsonRet['WARN']   := cMessage
				U_pFwLog("ERROR","PayTra03",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+cMessage)
				return oJsonRet
			endIf

			BeginSql Alias cAliasPay
			    SELECT
			        E1_XPAYID
			    FROM
			        %Table:SE1% SE1
			    WHERE
			            SE1.E1_FILIAL = %xFilial:SE1%
			        AND SE1.E1_XPAYID = %Exp:cValToChar(oParseJSON:Titulos[_a]:NUMERO)%
			        AND SE1.E1_TIPO = %Exp:cValToChar(oParseJSON:Titulos[_a]:TIPO)%
        			AND SE1.%NotDel%
			EndSql

			cMessage := GetLastQuery()[2] //RETORNA ULTIMA QUERY EXECUTADA
			U_pFwLog("INFO","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][INFO] - '+cMessage)

			(cAliasPay)->(DBGoTop())
			If (cAliasPay)->( EOF() )
				cIdPay := oParseJSON:Titulos[_a]:NUMERO
				//Prepara o array para o execauto
				aVetSE1 := {}
				aAdd(aVetSE1, {"E1_FILIAL",  FWxFilial("SE1"),  Nil})
				aAdd(aVetSE1, {"E1_NUM",     oParseJSON:Titulos[_a]:NUMERO,             Nil})
				aAdd(aVetSE1, {"E1_PREFIXO", oParseJSON:Titulos[_a]:PREFIXO,            Nil})
				aAdd(aVetSE1, {"E1_PARCELA", oParseJSON:Titulos[_a]:PARCELA,            Nil})
				aAdd(aVetSE1, {"E1_TIPO",    oParseJSON:Titulos[_a]:TIPO,               Nil})
				aAdd(aVetSE1, {"E1_NATUREZ", oParseJSON:Titulos[_a]:NATUREZA,           Nil})
				aAdd(aVetSE1, {"E1_CLIENTE", SA1->A1_COD,                               Nil})
				aAdd(aVetSE1, {"E1_LOJA",    SA1->A1_LOJA,                              Nil})
				aAdd(aVetSE1, {"E1_NOMCLI",  SA1->A1_NOME,                              Nil})
				aAdd(aVetSE1, {"E1_EMISSAO", stod(oParseJSON:Titulos[_a]:EMISSAO),      Nil})
				aAdd(aVetSE1, {"E1_VENCTO",  stod(oParseJSON:Titulos[_a]:VENCTO),       Nil})
				aAdd(aVetSE1, {"E1_VALOR",   oParseJSON:Titulos[_a]:VALOR,            Nil})
				aAdd(aVetSE1, {"E1_HIST",    oParseJSON:Titulos[_a]:HISTORICO,             Nil})
				aAdd(aVetSE1, {"E1_XPAYID",  oParseJSON:Titulos[_a]:NUMERO,             Nil})

				ARatEvEz:={}
				if oParseJSON:Titulos[_a]:MULTINAT == '1'
					if !AttIsMemberOf(oParseJSON:Titulos[_a],"RATNAT")
						cMessage := 'Atributo RATNAT nao informado no corpo da requisicao, Necessario informar quando atributo MULTINAT for igual 1'
						oJsonRet['STATUS'] := .F.
						oJsonRet['WARN']   := cMessage
						U_pFwLog("ERROR","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][ERROR] - '+cMessage)
						return oJsonRet
					endIf
					for _n:=1 to len(oParseJSON:Titulos[_a]:RATNAT)
						aAuxEv:={}
						aRatEz := {}

						if     !AttIsMemberOf(oParseJSON:Titulos[_a]:RATNAT[_n],"NATUREZA")
							lError   := .T.
							cMessage := 'Atributo NATUREZA dentro do array de objetos RATNAT nao foi informado no corpo da requisicao'
						elseif !AttIsMemberOf(oParseJSON:Titulos[_a]:RATNAT[_n],"VALORRAT")
							lError   := .T.
							cMessage := 'Atributo VALORRAT dentro do array de objetos RATNAT nao foi informado no corpo da requisicao'
						elseif !AttIsMemberOf(oParseJSON:Titulos[_a]:RATNAT[_n],"PERRAT")
							lError   := .T.
							cMessage := 'Atributo PERRAT dentro do array de objetos RATNAT nao foi informado no corpo da requisicao'
						elseif !AttIsMemberOf(oParseJSON:Titulos[_a]:RATNAT[_n],"RATCC")
							lError   := .T.
							cMessage := 'Atributo RATCC dentro do array de objetos RATNAT nao foi informado no corpo da requisicao'
						endIf

						if lError
							oJsonRet['STATUS'] := .F.
							oJsonRet['WARN']   := cMessage
							U_pFwLog("ERROR","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][ERROR] - '+cMessage)
							return oJsonRet
						endIf

						//Adicionando o vetor da natureza
						aadd( aAuxEv ,{"EV_NATUREZ" , oParseJSON:Titulos[_a]:RATNAT[_n]:NATUREZA, Nil })//natureza a ser rateada
						aadd( aAuxEv ,{"EV_VALOR" ,   oParseJSON:Titulos[_a]:RATNAT[_n]:VALORRAT, Nil })//valor do rateio na natureza
						aadd( aAuxEv ,{"EV_PERC" ,    oParseJSON:Titulos[_a]:RATNAT[_n]:PERRAT, Nil })//percentual do rateio na natureza
						aadd( aAuxEv ,{"EV_RATEICC" , cValtochar(oParseJSON:Titulos[_a]:RATNAT[_n]:RATCC), Nil })//indicando que há rateio por centro de custo

						//primeiro centro de custo
						if oParseJSON:Titulos[_a]:RATNAT[_n]:RATCC == '1'
							for _c:=1 to len(oParseJSON:Titulos[_a]:RATNAT[_n]:ARATCC)

								if     !AttIsMemberOf(oParseJSON:Titulos[_a]:RATNAT[_n]:ARATCC[_c],"CC")
									lError   := .T.
									cMessage := 'Atributo CC dentro do array de objetos ARATCC nao foi informado no corpo da requisicao'
								elseif !AttIsMemberOf(oParseJSON:Titulos[_a]:RATNAT[_n]:ARATCC[_c],"CCITEM")
									lError   := .T.
									cMessage := 'Atributo CCITEM dentro do array de objetos ARATCC nao foi informado no corpo da requisicao'
								elseif !AttIsMemberOf(oParseJSON:Titulos[_a]:RATNAT[_n]:ARATCC[_c],"CLVLDB")
									lError   := .T.
									cMessage := 'Atributo CLVLDB dentro do array de objetos ARATCC nao foi informado no corpo da requisicao'
								elseif !AttIsMemberOf(oParseJSON:Titulos[_a]:RATNAT[_n]:ARATCC[_c],"VALORRAT")
									lError   := .T.
									cMessage := 'Atributo VALORRAT dentro do array de objetos ARATCC nao foi informado no corpo da requisicao'
								elseif !AttIsMemberOf(oParseJSON:Titulos[_a]:RATNAT[_n]:ARATCC[_c],"PERRAT")
									lError   := .T.
									cMessage := 'Atributo PERRAT dentro do array de objetos ARATCC nao foi informado no corpo da requisicao'
								endIf

								if lError
									oJsonRet['STATUS'] := .F.
									oJsonRet['WARN']   := cMessage
									U_pFwLog("ERROR","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][ERROR] - '+cMessage)
									return oJsonRet
								endIf

								aAuxEz:={}
								aadd( aAuxEz ,{"EZ_CCUSTO" ,  oParseJSON:Titulos[_a]:RATNAT[_n]:ARATCC[_c]:CC, Nil })//centro de custo da natureza
								aadd( aAuxEz ,{"EZ_ITEMCTA",  oParseJSON:Titulos[_a]:RATNAT[_n]:ARATCC[_c]:CCITEM, Nil })//centro de custo da natureza
								aadd( aAuxEz ,{"EZ_CLVL",     oParseJSON:Titulos[_a]:RATNAT[_n]:ARATCC[_c]:CLVLDB, Nil })//centro de custo da natureza
								aadd( aAuxEz ,{"EZ_VALOR",    oParseJSON:Titulos[_a]:RATNAT[_n]:ARATCC[_c]:VALORRAT, Nil })//valor do rateio neste centro de custo
								aadd( aAuxEz ,{"EZ_PERC",     oParseJSON:Titulos[_a]:RATNAT[_n]:ARATCC[_c]:PERRAT, Nil })//valor do rateio neste centro de custo
								aadd(aRatEz,aAuxEz)
							NEXT _c
						EndIf

						aadd(aAuxEv,{"AUTRATEICC" , aRatEz, Nil })//recebendo dentro do array da natureza os multiplos centros de custo
						aAdd(aRatEvEz,aAuxEv)//adicionando a natureza ao rateio de multiplas naturezas

					Next _n
					aAdd(aArray,{"AUTRATEEV",ARatEvEz,Nil})//adicionando ao vetor aCab o vetor do rateio
				Else
					if oParseJSON:Titulos[_a]:NATUREZA <> ''
						aAdd(aArray,{ "E2_NATUREZ" , oParseJSON:Titulos[_a]:NATUREZA, NIL })
					endIf
				EndIf

				if alltrim(oParseJSON:Titulos[_a]:TIPO)=='RA'

					if !AttIsMemberOf(oParseJSON:Titulos[_a],"BANCO")
						lError   := .T.
						cMessage := 'Atributo BANCO nao informado no corpo da requisicao'
					elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"AGENCIA")
						lError   := .T.
						cMessage := 'Atributo AGENCIA nao informado no corpo da requisicao'
					elseif !AttIsMemberOf(oParseJSON:Titulos[_a],"CONTA")
						lError   := .T.
						cMessage := 'Atributo CONTA nao informado no corpo da requisicao'
					endIf

					if lError
						oJsonRet['STATUS'] := .F.
						oJsonRet['WARN']   := cMessage
						U_pFwLog("ERROR","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][ERROR] - '+cMessage)
						return oJsonRet
					endIf

					U_pFwLog("INFO","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][INFO] - BANCO')

					Private cBancoAdt   := oParseJSON:Titulos[_a]:BANCO
					Private cAgenciaAdt := oParseJSON:Titulos[_a]:AGENCIA
					Private cNumCon     := oParseJSON:Titulos[_a]:CONTA

					aAdd(aArray,{ "CBCOAUTO" , oParseJSON:Titulos[_a]:BANCO, NIL }) //CBCOAUTO
					aAdd(aArray,{ "CAGEAUTO" , oParseJSON:Titulos[_a]:AGENCIA, NIL }) //CAGEAUTO
					aAdd(aArray,{ "CCTAAUTO" , oParseJSON:Titulos[_a]:CONTA, NIL }) //CCTAAUTO

				endIf

				lMsErroAuto := .F.
				BEGIN SEQUENCE

					MSExecAuto({|x,y| FINA040(x,y)}, aVetSE1, 3)

					if lMsErroAuto
						cMessage :='ERRO ROTINA AUTOMATICA CADASTRO DE TITULO REEMBOLSO'
						U_pFwLog("ERROR","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][ERROR] - '+cMessage)

						aMsg := GetAutoGRLog()
						aEval(aMsg,{ |x| cErro += x + CRLF })
						oJsonRet['STATUS'] := .F.
						oJsonRet['WARN']   := U_pTrataErro(cErro)
						Return oJsonRet

					else
						oJsonRet['STATUS']           := .T.
						oJsonRet['TITULO_DEVOLUCAO'] := JsonObject():New()
						oJsonRet['TITULO_DEVOLUCAO'] := 'Inclusao de Titulo de Devolucao'
					endIf

				END SEQUENCE

				ErrorBlock(bError)

				If (!Empty(cError))
					U_pFwLog("ERROR","PayTra12",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+Repl("-", 80))
					U_pFwLog("ERROR","PayTra12",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+PadC("Error: " + AllTrim(cError), 80))
					U_pFwLog("ERROR","PayTra12",'[PAYTRACK][TITULOREEMBOLSO][ERROR] - '+Repl("-", 80))
				EndIf

			else
				oJsonRet['STATUS']  := .F.
				oJsonRet['WARN']    := "Duplicidade de Registro. Titulo ja cadastrado."
				cMessage := 'TITULO DEVOLUCAO JA CADASTRADO'
				U_pFwLog("ERROR","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][ERROR] - '+cMessage)
				exit

			endIf

			(cAliasPay)->(dbCloseArea())
		Next a
	else
		U_pFwLog("ERROR","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][ERROR] - ARRAY DE OBJETOS - TITULOS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO')
		oJsonRet['STATUS'] := .F.
		oJsonRet['WARN']   := " ARRAY DE OBJETOS - TITULOS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO "
	endIf

	U_pFwLog("INFO","PayTra12",'[PAYTRACK][TITULODEVOLUCAO][INFO] - FIM  ROTINA TITULO DEVOLUCAO')


Return oJsonRet
