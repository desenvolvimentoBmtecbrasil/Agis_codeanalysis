#INCLUDE "TOTVS.CH"

/*/{Protheus.doc} PayTra14

Inclusao Pre-Nota

@type function
@author Samuel Schneider - BMTEC
@since 19/07/2023
@version P11,P12
@database SQL Server,Oracle

@history 19/07/2023, Liberacao inicial

@param cJson, string, String json com os dados enviados

@return nill

/*/
User Function PayTra14(oParse)
	
    Local oJsonRet      := JsonObject():New()
	Local cMessage      := ""
    Local cErro         := ""
    Local aMsg          := {}
	Local nOpc          := 3
	Local nX            := 0
    Local nItem         := 0
	Local cSC7Key       := ''
	Local lLoopC7       := .F.
    
    Local cError := Space(0)
	Local bError := ErrorBlock({|oError| cError := oError:Description})

    Local cJson 			:= oParse:ToJson()
	Local oParseJSON 		:= Nil
    
	Private aCabec      := {}
	Private aItens      := {}
	Private aLinha      := {}
	   
    Private lMsErroAuto := .F.
	Private lAutoErrNoFile := .T.

	U_pFwLog("INFO","PayTra14",'[PAYTRACK][PRENOTA][INFO] - INICIO ROTINA DE PRE NOTA')

	oJsonRet['STATUS'] := JsonObject():New()
	oJsonRet['WARN']   := JsonObject():New()

    FWJsonDeserialize(DecodeUtf8(cJson),@oParseJSON)

    if     !AttIsMemberOf(oParseJSON,"DOCUMENTO")
        lError   := .T.
		cMessage := 'Atributo DOCUMENTO nao informado no corpo da requisicao'
	elseif !AttIsMemberOf(oParseJSON,"SERIE")
		lError   := .T.
		cMessage := 'Atributo SERIE nao informado no corpo da requisicao'
	elseIf  !AttIsMemberOf(oParseJSON,"FORNECEDOR")
		lError   := .T.
		cMessage := 'Atributo FORNECEDOR nao informado no corpo da requisicao'
    elseIf  !AttIsMemberOf(oParseJSON,"EMISSAO")
		lError   := .T.
		cMessage := 'Atributo EMISSAO nao informado no corpo da requisicao'
	endif

	if lError
		oJsonRet['STATUS'] := .F.
		oJsonRet['WARN']   := cMessage
		U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
		return oJsonRet
	endIf
    
    If !Empty(oParseJSON:ITENS)

        dbSelectArea("SA2")
        dbSetOrder(3)
        if !dbSeek(xFilial("SA2")+PADR(Alltrim(oParseJSON:FORNECEDOR),TamSx3('A2_CGC')[1]))
            cMessage           := 'Fornecedor nao cadastrado [CGC]: '+PadR( ALLTRIM(oParseJSON:FORNECEDOR), TamSX3("A2_CGC")[01] )
            oJsonRet['STATUS'] := .F.
            oJsonRet['WARN']   := cMessage
            U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
            Return oJsonRet
        endIf
       
        aAdd(aCabec,{"F1_SERIE",Padr(Alltrim(oParseJSON:SERIE),TamSx3("F1_SERIE")[1]),NIL})
        aAdd(aCabec,{"F1_EMISSAO",CTOD(oParseJSON:EMISSAO),NIL})
        aAdd(aCabec,{'F1_FORNECE',SA2->A2_COD,NIL})
        aAdd(aCabec,{'F1_LOJA',SA2->A2_LOJA,NIL})

        For nX := 1 To Len(oParseJSON:ITENS)
            nItem := 0
            lLoopC7 := .F. //Essa varaivel controla se foi informado apenas o numero do pedido no array de ITENS - neste caso eh necessario entrar no while do pedido para buscar todos os itens
            
            if  !AttIsMemberOf(oParseJSON:ITENS[nX],"PEDIDO")    == 'U'
                lError   := .T.
		        cMessage := 'Atributo PEDIDO dentro do array de Objetos ITENS nao informado no corpo da requisicao'
            elseIf !AttIsMemberOf(oParseJSON:ITENS[nX],"ITEM")   == 'U'
                lError   := .T.
		        cMessage := 'Atributo ITEM dentro do array de Objetos ITENS nao informado no corpo da requisicao'            
            elseIf !AttIsMemberOf(oParseJSON:ITENS[nX],"CODIGO") == 'U'
                lError   := .T.
		        cMessage := 'Atributo CODIGO dentro do array de Objetos ITENS nao informado no corpo da requisicao' 
            elseIf !AttIsMemberOf(oParseJSON:ITENS[nX],"QUANT")  == 'U'
                lError   := .T.
		        cMessage := 'Atributo QUANT dentro do array de Objetos ITENS nao informado no corpo da requisicao' 
            elseIf !AttIsMemberOf(oParseJSON:ITENS[nX],"VUNIT")  == 'U'
                lError   := .T.
		        cMessage := 'Atributo VUNIT dentro do array de Objetos ITENS nao informado no corpo da requisicao' 
            elseIf !AttIsMemberOf(oParseJSON:ITENS[nX],"TOTAL")  == 'U'
                lError   := .T.
		        cMessage := 'Atributo TOTAL dentro do array de Objetos ITENS nao informado no corpo da requisicao' 
            endIf

            if lError
                oJsonRet['STATUS'] := .F.
                oJsonRet['WARN']   := cMessage
                U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
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
                    U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)

                    Return oJsonRet
                    
                else
                    if lLoopC7
                        do while SC7->(!Eof()) .and. SC7->C7_FILIAL+SC7->C7_NUM = xFilial('SC7')+csc7kEY
                            nItem++
						    aAdd(aItens,{'D1_ITEM',StrZero(nItem,4),NIL})
                            aAdd(aItens,{'D1_COD',SC7->C7_PRODUTO,NIL})
                            if  oParseJSON:ITENS[nX]:QUANT > 0 .and.  valtype(oParseJSON:ITENS[nX]:QUANT) == 'N'
                                aAdd(aItens,{"D1_QUANT",oParseJSON:ITENS[nX]:QUANT,Nil})
                            else
                                aAdd(aItens,{"D1_QUANT",SC7->C7_QUANT,Nil})
                            endIf
                            if  oParseJSON:ITENS[nX]:VUNIT > 0 .and.  valtype(oParseJSON:ITENS[nX]:VUNIT) == 'N'
                                aAdd(aItens,{"D1_VUNIT",oParseJSON:ITENS[nX]:VUNIT,Nil})
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
                            SC7->(dbSkip())
                        endDo
                    else
                        aAdd(aItens,{'D1_COD',SC7->C7_PRODUTO,NIL})
                        if  oParseJSON:ITENS[nX]:QUANT > 0 .and.  valtype(oParseJSON:ITENS[nX]:QUANT) == 'N'
                            aAdd(aItens,{"D1_QUANT",oParseJSON:ITENS[nX]:QUANT,Nil})
                        else
                            aAdd(aItens,{"D1_QUANT",SC7->C7_QUANT,Nil})
                        endIf
                        if  oParseJSON:ITENS[nX]:QUANT > 0 .and.  valtype(oParseJSON:ITENS[nX]:QUANT) == 'N'
                            aAdd(aItens,{"D1_VUNIT",oParseJSON:ITENS[nX]:VUNIT,Nil})
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
                    endIf

                endIf
            else
                //[D1_ITEM]
                aAdd(aItens,{'D1_ITEM',StrZero(nX,4),NIL})
                
                //[D1_COD]
                if Empty(oParseJSON:ITENS[nX]:CODIGO) //valida se foi informado o codigo do produto
                    cMessage := "Valide se o codigo do produto esta sendo enviado no corpo da requisicao"
                    oJsonRet['STATUS'] := .F.
                    oJsonRet['WARN']   := cMessage
                    U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
                    Return oJsonRet
                elseIf !fExistProd(oParseJSON:ITENS[nX]:CODIGO) //valida se o produto informado existe na base de dados do Protheus
                    cMessage := "Valide se o codigo do produto: "+alltrim(oParseJSON:ITENS[nX]:CODIGO)+" encontra-se cadastrado no ERP"
                    oJsonRet['STATUS'] := .F.
                    oJsonRet['WARN']   := cMessage
                    U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
                    Return oJsonRet
                elseIf fProdBlq(oParseJSON:ITENS[nX]:CODIGO) //valida se o produto informado esta bloqueado na base de dados do Protheus
                    cMessage := "Informe um produto que nao esteja bloqueado na base de dados ou altere o cadastro do produto: "+alltrim(oParseJSON:ITENS[nX]:CODIGO)+" no ERP"
                    oJsonRet['STATUS'] := .F.
                    oJsonRet['WARN']   := cMessage
                    U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
                    Return oJsonRet
                else
                    aAdd(aItens,{'D1_COD',Padr(Alltrim(oParseJSON:ITENS[nX]:CODIGO),TamSx3('D1_COD')[1]),NIL})
                endIf

                //[D1_QUANT]
                if valtype(oParseJSON:ITENS[nX]:QUANT) == 'C'
                    cMessage := "Valide se o atributo QUANT esta recebendo um valor numerico e reenvie a requisicao"
                    oJsonRet['STATUS'] := .F.
                    oJsonRet['WARN']   := cMessage
                    U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
                    Return oJsonRet
                elseIf oParseJSON:ITENS[nX]:QUANT <= 0
                    cMessage := "Valide a quantidade informada no atributo QUANT , ela deve ser maior que zero. Reenvie a requisicao"
                    oJsonRet['STATUS'] := .F.
                    oJsonRet['WARN']   := cMessage
                    U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
                    Return oJsonRet
                else
                    aAdd(aItens,{"D1_QUANT",oParseJSON:ITENS[nX]:QUANT,Nil})
                endIf

                //[D1_VUNIT]
                if valtype(oParseJSON:ITENS[nX]:VUNIT) == 'C'
                    cMessage := "Valide se o atributo VUNIT esta recebendo um valor numerico e reenvie a requisicao"
                    oJsonRet['STATUS'] := .F.
                    oJsonRet['WARN']   := cMessage
                    U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
                    Return oJsonRet
                elseIf oParseJSON:ITENS[nX]:VUNIT <= 0
                    cMessage := "Valide o valor informada no atributo VUNIT , ela deve ser maior que zero. Reenvie a requisicao"
                    oJsonRet['STATUS'] := .F.
                    oJsonRet['WARN']   := cMessage
                    U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
                    Return oJsonRet
                else
                    aAdd(aItens,{"D1_VUNIT",oParseJSON:ITENS[nX]:VUNIT,Nil})
                endIf

                //[D1_TOTAL]
                if valtype(oParseJSON:ITENS[nX]:TOTAL) == 'C'
                    cMessage := "Valide se o atributo TOTAL esta recebendo um valor numerico e reenvie a requisicao"
                    oJsonRet['STATUS'] := .F.
                    oJsonRet['WARN']   := cMessage
                    U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
                    Return oJsonRet
                elseIf oParseJSON:ITENS[nX]:TOTAL <= 0
                    cMessage :="Valide o valor informada no atributo TOTAL , ela deve ser maior que zero. Reenvie a requisicao"
                    oJsonRet['STATUS'] := .F.
                    oJsonRet['WARN']   := cMessage
                    U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+cMessage)
                    Return oJsonRet
                else
                    aAdd(aItens,{"D1_TOTAL",oParseJSON:ITENS[nX]:TOTAL,Nil})
                endIf

            endIf

            aAdd(aLinha,aItens)
            aItens := {}

        Next nX
        
        lMsErroAuto := .F.
        BEGIN TRANSACTION 
            BEGIN SEQUENCE
                MSExecAuto({|x,y,z,a,b| MATA140(x,y,z,a,b)}, aCabec, aLinha, nOpc,,)
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
                    oJsonRet['PRE_NOTA'] := 'Pre-nota incluida com sucesso ['+SF1->F1_DOC+'/'+SF1->F1_SERIE+']'
                EndIf
            END SEQUENCE
        END TRANSACTION

		ErrorBlock(bError)

		If (!Empty(cError))
			U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+Repl("-", 80))
			U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+PadC("Error: " + AllTrim(cError), 80))
			U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - '+Repl("-", 80))
		EndIf
    else
        U_pFwLog("ERROR","PayTra14",'[PAYTRACK][PRENOTA][ERROR] - ARRAY DE OBJETOS - ITENS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO')
        oJsonRet['STATUS'] := .F.
        oJsonRet['WARN']   := " ARRAY DE OBJETOS - ITENS - NAO FOI INFORMADO OU POPULADO NO CORPO DA REQUISICAO "
    endIf

    U_pFwLog("INFO","PayTra14",'[PAYTRACK][PRENOTA][INFO] - FIM ROTINA DE PRE NOTA')

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

