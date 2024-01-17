#include "Totvs.ch"

/*/{Protheus.doc} PayTra04

Inclusao NFE

@type function
@author Katieli de Oliviera - BMTEC
@since 04/01/2022
@version P11,P12
@database SQL Server,Oracle

@history 04/01/2022, Liberacao inicial
@history 21/07/2023, Refatorado

@param oParseJSON, objeto, Objeto json com os dados enviados

@return caracter, Erro

@see WsPayTrac01
@see PayTra01
@see PayTra02
@see PayTra03
@see PayTra05
@see PayTra06

/*/

User Function PayTra04(oParse)
	Local oJsonRet := JsonObject():New()
	Local cErro  := ""
	Local nOpc   := 3
	Local aCab   := {}
	Local aItens := {}
	Local aLinha := {}
	Local nX     := 0
	Local cError := Space(0)
	Local bError := ErrorBlock({|oError| cError := oError:Description})
	Local cMessage := ''
	Local nItem   := 0
	Local cJson 			:= oParse:ToJson()
	Local oParseJSON 		:= Nil

	Private lMsErroAuto := .F.
	Private lAutoErrNoFile := .T.


	U_pFwLog("INFO","PayTra04",'[PAYTRACK][NFREEMBOLSO][INFO] - INICIO ROTINA NF REEMBOLSO')

	oJsonRet['STATUS'] := JsonObject():New()
	oJsonRet['WARN']   := JsonObject():New()

	FWJsonDeserialize(DecodeUtf8(cJson),@oParseJSON)

	if      !AttIsMemberOf(oParseJSON,"DOCUMENTO")       	
		lError   := .T.
		cMessage := 'Atributo DOCUMENTO nao informado no corpo da requisicao'
	elseif  !AttIsMemberOf(oParseJSON,"SERIE")       	
		lError   := .T.
		cMessage := 'Atributo SERIE nao informado no corpo da requisicao'
	elseIf !AttIsMemberOf(oParseJSON,"FORNECEDOR")	
		lError   := .T.
		cMessage := 'Atributo FORNECEDOR nao informado no corpo da requisicao'
	elseIf !AttIsMemberOf(oParseJSON,"EMISSAO")	
		lError   := .T.
		cMessage := 'Atributo EMISSAO nao informado no corpo da requisicao'
	elseIf !AttIsMemberOf(oParseJSON,"ESPECIE")	
		lError   := .T.
		cMessage := 'Atributo ESPECIE nao informado no corpo da requisicao'
	endif

	if lError
		oJsonRet['STATUS'] := .F.
		oJsonRet['WARN']   := cMessage
		U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
		return oJsonRet
	endIf

	If !Empty(oParseJSON:ITENS)
		dbSelectArea("SA2")
		dbSetOrder(3)
		If !SA2->( dbSeek( xFilial("SA2") + PadR( ALLTRIM(oParseJSON:FORNECEDOR), TamSX3("A2_CGC")[01] ) ) )
			cMessage           := 'Fornecedor nao cadastrado [CGC]: '+PadR( ALLTRIM(oParseJSON:FORNECEDOR), TamSX3("A2_CGC")[01] )
			oJsonRet['STATUS'] := .F.
			oJsonRet['WARN']   := cMessage
			U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
			Return oJsonRet
		EndIf

		dbSelectArea("SE4")
		dbSetOrder(1)
		If !SE4->( dbSeek( xFilial( "SE4" ) + PadR( Alltrim(GetMv('ES_WSPAY03')), TamSX3("F1_COND")[01] ) ) )
			cMessage           := 'Condicao de pagamento nao cadastrada -->CODIGO: '+PadR( Alltrim(GetMv('ES_WSPAY03')), TamSX3("F1_COND")[01])
			oJsonRet['STATUS'] := .F.
			oJsonRet['WARN']   := cMessage
			U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
			Return oJsonRet
		EndIf

		aadd(aCab,{"F1_FILIAL"  ,cFilAnt 					,NIL})
		aadd(aCab,{"F1_TIPO"    ,"N" 						,NIL})
		aadd(aCab,{"F1_FORMUL"  ,"N" 						,NIL})
		aadd(aCab,{"F1_DOC"     ,oParseJSON:DOCUMENTO  			,NIL})
		aadd(aCab,{"F1_SERIE"   ,oParseJSON:SERIE 			,NIL})
		aadd(aCab,{"F1_EMISSAO" ,CTOD(oParseJSON:EMISSAO) 	,NIL})
		aadd(aCab,{"F1_DTDIGIT" ,CTOD(oParseJSON:EMISSAO) 	,NIL})
		aadd(aCab,{"F1_FORNECE" ,SA2->A2_COD	 			,NIL})
		aadd(aCab,{"F1_LOJA"    ,SA2->A2_LOJA 				,NIL})
		aadd(aCab,{"F1_ESPECIE" ,oParseJSON:ESPECIE 		,NIL})
		aadd(aCab,{"F1_COND"    ,SE4->E4_CODIGO 			,NIL})
		aadd(aCab,{"E2_NATUREZ"	,GetMv('ES_WSPAY01')		,Nil})

		For nX := 1 To Len(oParseJSON:ITENS)

			lLoopC7 := .F. //Essa varaivel controla se foi informado apenas o numero do pedido no array de ITENS - neste caso eh necessario entrar no while do pedido para buscar todos os itens
			nItem   := 0

			if  !AttIsMemberOf(oParseJSON:ITENS[nX],"PEDIDO")    == 'U'
				lError   := .T.
				cMessage := 'Atributo PEDIDO dentro do array de Objetos ITENS nao informado no corpo da requisicao'
			elseIf !AttIsMemberOf(oParseJSON:ITENS[nX],"ITEM")   == 'U'
				lError   := .T.
				cMessage := 'Atributo ITEM dentro do array de Objetos ITENS nao informado no corpo da requisicao'
			elseIf !AttIsMemberOf(oParseJSON:ITENS[nX],"PRODUTO") == 'U'
				lError   := .T.
				cMessage := 'Atributo PRODUTO dentro do array de Objetos ITENS nao informado no corpo da requisicao'
			elseIf !AttIsMemberOf(oParseJSON:ITENS[nX],"QUANT")  == 'U'
				lError   := .T.
				cMessage := 'Atributo QUANT dentro do array de Objetos ITENS nao informado no corpo da requisicao'
			elseIf !AttIsMemberOf(oParseJSON:ITENS[nX],"PRECO")  == 'U'
				lError   := .T.
				cMessage := 'Atributo PRECO dentro do array de Objetos ITENS nao informado no corpo da requisicao'
			elseIf !AttIsMemberOf(oParseJSON:ITENS[nX],"TOTAL")  == 'U'
				lError   := .T.
				cMessage := 'Atributo TOTAL dentro do array de Objetos ITENS nao informado no corpo da requisicao'
			elseIf !AttIsMemberOf(oParseJSON:ITENS[nX],"CC")  == 'U'
				lError   := .T.
				cMessage := 'Atributo CC dentro do array de Objetos ITENS nao informado no corpo da requisicao'
			endIf

			if lError
				oJsonRet['STATUS'] := .F.
				oJsonRet['WARN']   := cMessage
				U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
				return oJsonRet
			endIf

			if !EMPTY(oParseJSON:ITENS[nX]:PEDIDO) //VERIFICA SE FOI INFORMADO PEDIDO
				csc7kEY := Padr(Alltrim(oParseJSON:ITENS[nX]:PEDIDO),TamSx3('C7_NUM')[1])
				if !empty(oParseJSON:ITENS[nX]:ITEM)
					csc7kEY+=Padr(Alltrim(oParseJSON:ITENS[nX]:ITEM),TamSx3('D1_ITEM')[1])
				else
					lLoopC7 := .T.
				endIf
				dbSelectArea('SC7')
				SC7->(dbSetOrder(1))
				if !SC7->(dbSeek(xFilial('SC7')+csc7kEY))
					cMessage := "Valide se o pedido: "+Alltrim(oParseJSON:ITENS[nX]:PEDIDO)+;
						iif(!empty(Alltrim(oParseJSON:ITENS[nX]:ITEM))," item: "+Alltrim(oParseJSON:ITENS[nX]:ITEM)+" encontram-se cadastrado no ERP"," encontra-se cadastrado no ERP")
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)

					Return oJsonRet
				else
					if lLoopC7 // Foi informado soh o pedido
						do while SC7->(!Eof()) .and. SC7->C7_FILIAL+SC7->C7_NUM = xFilial('SC7')+csc7kEY
							nItem++
							aAdd(aItens,{'D1_ITEM',StrZero(nItem,4),NIL})
							aAdd(aItens,{'D1_COD',SC7->C7_PRODUTO,NIL})
							if  oParseJSON:ITENS[nX]:QUANT > 0 .and.  valtype(oParseJSON:ITENS[nX]:QUANT) == 'N'
								aAdd(aItens,{"D1_QUANT",oParseJSON:ITENS[nX]:QUANT,Nil})
							else
								aAdd(aItens,{"D1_QUANT",SC7->C7_QUANT,Nil})
							endIf
							if  oParseJSON:ITENS[nX]:PRECO > 0 .and.  valtype(oParseJSON:ITENS[nX]:PRECO) == 'N'
								aAdd(aItens,{"D1_VUNIT",oParseJSON:ITENS[nX]:PRECO,Nil})
							else
								aAdd(aItens,{"D1_VUNIT",SC7->C7_PRECO,Nil})
							endIf
							if  oParseJSON:ITENS[nX]:TOTAL > 0 .and.  valtype(oParseJSON:ITENS[nX]:TOTAL) == 'N'
								aAdd(aItens,{"D1_TOTAL",oParseJSON:ITENS[nX]:TOTAL,Nil})
							else
								aAdd(aItens,{"D1_TOTAL",SC7->C7_TOTAL,Nil})
							endIf
							aAdd(aItens,{"D1_PEDIDO",SC7->C7_NUM,NIL})
							aAdd(aItens,{"D1_ITEMPC",SC7->C7_ITEM,NIL})

							if  !empty(oParseJSON:ITENS[nX]:CC)  .and.  valtype(oParseJSON:ITENS[nX]:CC) == 'C'
								aAdd(aItens,{"D1_CC",oParseJSON:ITENS[nX]:CC,Nil})
							else
								aAdd(aItens,{"D1_CC",SC7->C7_CC,NIL})
							endIf

							dbSelectArea("SB1")
							dbSetOrder(1)
							If SB1->(dbSeek( xFilial( "SB1" )+SC7->C7_PRODUTO))

								aadd(aItens,{"D1_UM"      ,SB1->B1_UM,NIL})
								aadd(aItens,{"D1_LOCAL"   ,SB1->B1_LOCPAD,NIL})
								aadd(aItens,{"D1_TES"     ,GetMv('ES_WSPAY02'),NIL})

							endIf
							aAdd(aLinha,aItens)
							aItens := {}
							SC7->(dbSkip())
						endDo
					else //Foi informado pedido e item
						aAdd(aItens,{'D1_ITEM',StrZero(nX,4),NIL})
						aAdd(aItens,{'D1_COD',SC7->C7_PRODUTO,NIL})
						if  oParseJSON:ITENS[nX]:QUANT > 0 .and.  valtype(oParseJSON:ITENS[nX]:QUANT) == 'N'
							aAdd(aItens,{"D1_QUANT",oParseJSON:ITENS[nX]:QUANT,Nil})
						else
							aAdd(aItens,{"D1_QUANT",SC7->C7_QUANT,Nil})
						endIf
						if  oParseJSON:ITENS[nX]:PRECO > 0 .and.  valtype(oParseJSON:ITENS[nX]:PRECO) == 'N'
							aAdd(aItens,{"D1_VUNIT",oParseJSON:ITENS[nX]:PRECO,Nil})
						else
							aAdd(aItens,{"D1_VUNIT",SC7->C7_PRECO,Nil})
						endIf
						if  oParseJSON:ITENS[nX]:TOTAL > 0 .and.  valtype(oParseJSON:ITENS[nX]:TOTAL) == 'N'
							aAdd(aItens,{"D1_TOTAL",oParseJSON:ITENS[nX]:TOTAL,Nil})
						else
							aAdd(aItens,{"D1_TOTAL",SC7->C7_TOTAL,Nil})
						endIf

						dbSelectArea("SB1")
						dbSetOrder(1)
						If SB1->(dbSeek( xFilial( "SB1" )+SC7->C7_PRODUTO))

							aadd(aItens,{"D1_UM"      ,SB1->B1_UM,NIL})
							aadd(aItens,{"D1_LOCAL"   ,SB1->B1_LOCPAD,NIL})
							aadd(aItens,{"D1_TES"     ,GetMv('ES_WSPAY02'),NIL})

						endIf

						aAdd(aItens,{"D1_PEDIDO",SC7->C7_NUM,NIL})
						aAdd(aItens,{"D1_ITEMPC",SC7->C7_ITEM,NIL})

						if  !empty(oParseJSON:ITENS[nX]:CC) .and.  valtype(oParseJSON:ITENS[nX]:CC) == 'C'
							aAdd(aItens,{"D1_CC",oParseJSON:ITENS[nX]:CC,Nil})
						else
							aAdd(aItens,{"D1_CC",SC7->C7_CC,NIL})
						endIf

						aAdd(aLinha,aItens)
						aItens := {}

					endIf

				endIf
			else
				//[D1_ITEM]
				aAdd(aItens,{'D1_ITEM',StrZero(nX,4),NIL})

				//[D1_COD]
				if Empty(oParseJSON:ITENS[nX]:PRODUTO) //valida se foi informado o codigo do produto
					cMessage := "Valide se o codigo do produto esta sendo enviado no corpo da requisicao"
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
					Return oJsonRet
				elseIf !fExistProd(oParseJSON:ITENS[nX]:PRODUTO) //valida se o produto informado existe na base de dados do Protheus
					cMessage := "Valide se o codigo do produto: "+alltrim(oParseJSON:ITENS[nX]:PRODUTO)+" encontra-se cadastrado no ERP"
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
					Return oJsonRet
				elseIf fProdBlq(oParseJSON:ITENS[nX]:PRODUTO) //valida se o produto informado esta bloqueado na base de dados do Protheus
					cMessage := "Informe um produto que nao esteja bloqueado na base de dados ou altere o cadastro do produto: "+alltrim(oParseJSON:ITENS[nX]:PRODUTO)+" no ERP"
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
					Return oJsonRet
				else

					aadd(aItens,{"D1_COD"     ,SB1->B1_COD,NIL})
					aadd(aItens,{"D1_UM"      ,SB1->B1_UM,NIL})
					aadd(aItens,{"D1_LOCAL"   ,SB1->B1_LOCPAD,NIL})
					aadd(aItens,{"D1_TES"     ,GetMv('ES_WSPAY02'),NIL})

				endIf

				//[D1_QUANT]
				if valtype(oParseJSON:ITENS[nX]:QUANT) == 'C'
					cMessage := "Valide se o atributo QUANT esta recebendo um valor numerico e reenvie a requisicao"
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
					Return oJsonRet
				elseIf oParseJSON:ITENS[nX]:QUANT <= 0
					cMessage := "Valide a quantidade informada no atributo QUANT , ela deve ser maior que zero. Reenvie a requisicao"
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
					Return oJsonRet
				else
					aAdd(aItens,{"D1_QUANT",oParseJSON:ITENS[nX]:QUANT,Nil})
				endIf

				//[D1_VUNIT]
				if valtype(oParseJSON:ITENS[nX]:PRECO) == 'C'
					cMessage := "Valide se o atributo PRECO esta recebendo um valor numerico e reenvie a requisicao"
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
					Return oJsonRet
				elseIf oParseJSON:ITENS[nX]:PRECO <= 0
					cMessage :="Valide o valor informada no atributo PRECO , ela deve ser maior que zero. Reenvie a requisicao"
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
					Return oJsonRet
				else
					aAdd(aItens,{"D1_VUNIT",oParseJSON:ITENS[nX]:PRECO,Nil})
				endIf

				//[D1_TOTAL]
				if valtype(oParseJSON:ITENS[nX]:TOTAL) == 'C'
					cMessage := "Valide se o atributo TOTAL esta recebendo um valor numerico e reenvie a requisicao"
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
					Return oJsonRet
				elseIf oParseJSON:ITENS[nX]:TOTAL <= 0
					cMessage :="Valide o valor informada no atributo TOTAL , ela deve ser maior que zero. Reenvie a requisicao"
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
					Return oJsonRet
				else
					aAdd(aItens,{"D1_TOTAL",oParseJSON:ITENS[nX]:TOTAL,Nil})
				endIf

				//[D1_CC]
				if valtype(oParseJSON:ITENS[nX]:CC) == 'N'
					cMessage := "Valide o valor informada no atributo CC, ele deve ser do tipo caracter"
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
					Return oJsonRet
				elseIf !EMPTY(oParseJSON:ITENS[nX]:CC)
					cMessage := "Valide se o codigo do centro de custo [ATRIBUTO=CC] esta sendo enviado no corpo da requisicao"
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := cMessage
					U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+cMessage)
					Return oJsonRet
				else
					aadd(aItem,{"D1_CC",(oParseJSON:itens[nX]:CC),NIL})
				endIf

				aAdd(aLinha,aItens)
				aItens := {}

			endIf



		Next nX

		lMsErroAuto := .F.

		BEGIN TRANSACTION
			BEGIN SEQUENCE
				MSExecAuto({ |x,y,z| MATA103(x,y,z,,,,,,,,)},aCab,aLinha,nOpc,,)
				If lMsErroAuto
					DisarmTransaction()
					cMessage :='ERRO ROTINA AUTOMATICA CADASTRO DE PRE NOTA'
					U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
					aMsg := GetAutoGRLog()
					aEval(aMsg,{ |x| cErro += x + CRLF })
					oJsonRet['STATUS'] := .F.
					oJsonRet['WARN']   := U_pTrataErro(cErro)
					Return oJsonRet
				Else
					oJsonRet['STATUS']           := .T.
					oJsonRet['PRE_NOTA'] := JsonObject():New()
					oJsonRet['PRE_NOTA'] := 'Nf Reembolso incluida com sucesso ['+SF1->F1_DOC+'/'+SF1->F1_SERIE+']'
				EndIf
			END SEQUENCE
		END TRANSACTION

		ErrorBlock(bError)

		If (!Empty(cError))
			U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+Repl("-", 80))
			U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+PadC("Error: " + AllTrim(cError), 80))
			U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - '+Repl("-", 80))
		EndIf

	else
		U_pFwLog("ERROR","PayTra04",'[PAYTRACK][NFREEMBOLSO][ERROR] - ARRAY DE OBJETOS - ITENS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO')
		oJsonRet['STATUS'] := .F.
		oJsonRet['WARN']   := " ARRAY DE OBJETOS - ITENS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO "
	endIf

	U_pFwLog("INFO","PayTra04",'[PAYTRACK][NFREEMBOLSO][INFO] - FIM ROTINA NF REEMBOLSO')

Return oJsonRet

//[funcoes auxiliares]
	//Verifica se existe produto
Static Function fExistProd(cCod)
	Local lRet := .F.

	dbSelectArea('SB1')
	SB1->(dbSetOrder(1))
	If SB1->(dbSeek(xFilial('SB1')+Padr(alltrim(cCod),TamSx3('B1_COD')[1])))
		lRet := .T.
	endIf

Return lRet
	//Verifica se produto esta bloqueado
Static Function fProdBlq(cCod)
	Local lRet := .F.

	dbSelectArea('SB1')
	SB1->(dbSetOrder(1))
	If SB1->(dbSeek(xFilial('SB1')+Padr(alltrim(cCod),TamSx3('B1_COD')[1])))
		if SB1->B1_MSBLQL == '1'
			lRet := .T.
		endIf
	endIf

Return lRet


