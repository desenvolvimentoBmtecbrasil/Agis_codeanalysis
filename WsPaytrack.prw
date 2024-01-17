#Include 'RestFul.CH'
#include "Totvs.ch"


WsRestful WsPayTrac01 Description "Serviço REST de Saldos, Reembolso,Nf"

	//PADROES
        WsMethod GET CentroCusto;
            Description "Consulta Centro Custo";
            PATH "/api/v1/CentroCusto";
            WSSYNTAX "/api/v1/CentroCusto";
            PRODUCES APPLICATION_JSON

        WsMethod POST TituloReembolso;
            Description "Inclui Titulo de Reembolso";
            PATH "/api/v1/TituloReembolso";
            WSSYNTAX "/api/v1/TituloReembolso";
            PRODUCES APPLICATION_JSON

        WsMethod POST DevolTitulo;
            Description "Inclui Titulo de Dev";
            PATH "/api/v1/DevolTitulo";
            WSSYNTAX "/api/v1/DevolTitulo";
            PRODUCES APPLICATION_JSON

          WsMethod POST CompensaTitulos;
            Description "Compensa Titulos pelo ID";
            PATH "/api/v1/CompensaTitulos";
            WSSYNTAX "/api/v1/CompensaTitulos";
            PRODUCES APPLICATION_JSON 

        WsMethod POST PedidoCompra;
            Description "Inclui Pedido de Compras";
            PATH "/api/v1/PedidoCompra";
            WSSYNTAX "/api/v1/PedidoCompra";
            PRODUCES APPLICATION_JSON

        WsMethod POST PreNota;
            Description "Manutencao das Pre-Notas de entrada";
            PATH "/api/v1/PreNota";
            WSSYNTAX "/api/v1/PreNota";
            PRODUCES APPLICATION_JSON            
        
        WsMethod POST NfReembolso;
            Description "Inclui Nf de Reembolso";
            PATH "/api/v1/NfReembolso";
            WSSYNTAX "/api/v1/NfReembolso";
            PRODUCES APPLICATION_JSON
         

	//CUSTOMIZADOS
        WsMethod POST ValidaID;
            Description "Valida ID";
            PATH "/api/v1/ValidaID";
            WSSYNTAX "/api/v1/ValidaID";
            PRODUCES APPLICATION_JSON

        WsMethod POST Aprovadores;
            Description "Valida ID";
            PATH "/api/v1/Aprovadores";
            WSSYNTAX "/api/v1/Aprovadores";
            PRODUCES APPLICATION_JSON


END WsRestful

//PACOTE PADRAO
    
    // 1 - CENTRO DE CUSTO
    WSMETHOD GET CentroCusto WSREST WsPayTrac01

        Local cJson      := ::GetContent()
        Local oResponse  := JsonObject():New()
        Local oParse     := JsonObject():New()
        Local oDados     := Nil
        Local lOk        := .T.
        Local cDados     := ""
        Local cResponse  := ""
        Local cErr

        if u_isInTrans()
            lOk := .F.
            DisarmTransaction()
            SetRestFault(500, '{"erro - Thread em Aberto"}')
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][ERROR] - Thread em aberto...')
            Return (lOk)
        endIf

        // define o tipo de retorno do método
        ::SetContentType("application/json")

        cErr := oParse:fromJson(cJson)

        if ValType(cErr) == "U"
            U_pFwLog("INFO","WsPayTrac01",'[WsPayTrac01][CENTROCUSTO][SUCCESS] - Sucesso parser Json String')
            oDados := u_PayTra10(oParse)
        else
            lOk := .F.
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][CENTROCUSTO][ERROR] - Falha parser Json String')
            SetRestFault( 1, "Nao foi possivel processar a estrutura Json." )
            Return (lOk)
        endif

        If oDados['STATUS'] 
            
            U_pFwLog("INFO","WsPayTrac01",'[WsPayTrac01][CENTROCUSTO][SUCCESS] - Dados encontrados e retornados')
            
            oResponse["content"]            := JsonObject():New()
            oResponse["content"]["id"]	    := "01"
            oResponse["content"]["Type"]	:= "SUCCESS"
            oResponse["content"]["Message"]	:= oDados['CENTROS_CUSTO']

            cResponse := EncodeUTF8(oResponse:ToJson())
            ::SetResponse(cResponse)

        Else
            
            U_pFwLog("ERROR","WsPayTrac01",'[ERROR][CENTROCUSTO][SUCCESS] - Dados nao encontrados')

            oResponse["content"]            := JsonObject():New()
            oResponse["content"]["id"]	    := "A1"
            oResponse["content"]["Type"]	:= "ERRO"
            oResponse["content"]["Message"]	:= oDados['WARN']

            cDados :=  (EncodeUTF8(oResponse:ToJson()))
            SetRestFault(400,cDados)
            lOk := .F.

        EndIf

    Return( lOk )

    // 2 - REEMBOLSO
    WSMETHOD POST TituloReembolso WSREST WsPayTrac01
        Local cJson      := ::GetContent()
        Local oResponse  := JsonObject():New()
        Local oParse     := JsonObject():New()
        Local lOk        := .T.
        Local cErr

        if u_isInTrans()
            lOk := .F.
            DisarmTransaction()
            SetRestFault(500, '{"erro - Thread em Aberto"}')
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][TITULO_REEMBOLSO][ERROR] - Thread em aberto...')
            Return (lOk)
        endIf

        // define o tipo de retorno do método
        ::SetContentType("application/json")

        cErr := oParse:fromJson(cJson)

        if ValType(cErr) == "U"
            U_pFwLog("INFO","WsPayTrac01",'[WsPayTrac01][TITULO_REEMBOLSO][SUCCESS] - Sucesso parser Json String')
            oDados := u_PayTra03(oParse)
        else
            lOk := .F.
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][TITULO_REEMBOLSO][ERROR] - Falha parser Json String')
            SetRestFault( 1, "Nao foi possivel processar a estrutura Json." )
            Return (lOk)
        endif

        If oDados['STATUS']

            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "02"
            oResponse["content"]["Type"]	:= "SUCCESS"
            oResponse["content"]["Message"]	:= oDados['TITULO_REEMBOLSO']

            cResponse := EncodeUTF8(oResponse:ToJson())
            ::SetResponse(cResponse)

        Else
        
            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "A2"
            oResponse["content"]["Type"]	:= "ERRO"
            oResponse["content"]["Message"]	:= oDados['WARN']

            
            cErro :=  (EncodeUTF8(oResponse:ToJson()))
            SetRestFault(400, EncodeUTF8(cErro))
            lOk := .F.

        EndIf

    Return( lOk )

    // 3 - DEVOLUCAO
    WSMETHOD POST DevolTitulo WSREST WsPayTrac01
        Local cJson      := ::GetContent()
        Local oResponse  := JsonObject():New()
        Local oParse     := JsonObject():New()
        Local lOk        := .T.
        Local cErr

        if u_isInTrans()
            lOk := .F.
            DisarmTransaction()
            SetRestFault(500, '{"erro - Thread em Aberto"}')
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][TITULODEVOLUCAO][ERROR] - Thread em aberto...')
            Return (lOk)
        endIf

        // define o tipo de retorno do método
        ::SetContentType("application/json")

        cErr := oParse:fromJson(cJson)

        if ValType(cErr) == "U"
            U_pFwLog("INFO","WsPayTrac01",'[WsPayTrac01][TITULODEVOLUCAO][SUCCESS] - Sucesso parser Json String')
            oDados := u_PayTra12(oParse)
        else
            lOk := .F.
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][TITULODEVOLUCAO][ERROR] - Falha parser Json String')
            SetRestFault( 1, "Nao foi possivel processar a estrutura Json." )
            Return (lOk)
        endif

        If oDados['STATUS']

            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "03"
            oResponse["content"]["Type"]	:= "SUCCESS"
            oResponse["content"]["Message"]	:= oDados['TITULO_DEVOLUCAO']

            cResponse := EncodeUTF8(oResponse:ToJson())
            ::SetResponse(cResponse)

        Else
        
            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "A3"
            oResponse["content"]["Type"]	:= "ERRO"
            oResponse["content"]["Message"]	:= oDados['WARN']

            
            cErro :=  (EncodeUTF8(oResponse:ToJson()))
            SetRestFault(400, EncodeUTF8(cErro))
            lOk := .F.

        EndIf

    Return (lOk)

    // 4 - COMPENSACAO
    WSMETHOD POST CompensaTitulos WSREST WsPayTrac01
        Local cJson      := ::GetContent()
        Local oResponse  := JsonObject():New()
        Local oParse     := JsonObject():New()
        Local lOk        := .T.
        Local cErr

        if u_isInTrans()
            lOk := .F.
            DisarmTransaction()
            SetRestFault(500, '{"erro - Thread em Aberto"}')
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][COMPENSATITULO][ERROR] - Thread em aberto...')
            Return (lOk)
        endIf

        // define o tipo de retorno do método
        ::SetContentType("application/json")

        cErr := oParse:fromJson(cJson)

        if ValType(cErr) == "U"
            U_pFwLog("INFO","WsPayTrac01",'[WsPayTrac01][COMPENSATITULO][SUCCESS] - Sucesso parser Json String')
            oDados := u_PayTra11(oParse)
        else
            lOk := .F.
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][COMPENSATITULO][ERROR] - Falha parser Json String')
            SetRestFault( 1, "Nao foi possivel processar a estrutura Json." )
            Return (lOk)
        endif

        If oDados['STATUS']

            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "04"
            oResponse["content"]["Type"]	:= "SUCCESS"
            oResponse["content"]["Message"]	:= oDados['COMPENSA_TITULO']

            cResponse := EncodeUTF8(oResponse:ToJson())
            ::SetResponse(cResponse)

        Else
        
            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "A4"
            oResponse["content"]["Type"]	:= "ERRO"
            oResponse["content"]["Message"]	:= oDados['WARN']

            
            cErro :=  (EncodeUTF8(oResponse:ToJson()))
            SetRestFault(400, EncodeUTF8(cErro))
            lOk := .F.

        EndIf
    Return (lOk)

    // 5 - PEDIDO COMPRA
    WSMETHOD POST PedidoCompra WSREST WsPayTrac01
        Local cJson      := ::GetContent()
        Local oResponse  := JsonObject():New()
        Local oParse     := JsonObject():New()
        Local lOk        := .T.
        Local cErr

        if u_isInTrans()
            lOk := .F.
            DisarmTransaction()
            SetRestFault(500, '{"erro - Thread em Aberto"}')
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][PEDIDOCOMPRA][ERROR] - Thread em aberto...')
            Return (lOk)
        endIf

        // define o tipo de retorno do método
        ::SetContentType("application/json")

        cErr := oParse:fromJson(cJson)

        if ValType(cErr) == "U"
            U_pFwLog("INFO","WsPayTrac01",'[WsPayTrac01][PEDIDOCOMPRA][SUCCESS] - Sucesso parser Json String')
            oDados := u_PayTra05(oParse)
        else
            lOk := .F.
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][PEDIDOCOMPRA][ERROR] - Falha parser Json String')
            SetRestFault( 1, "Nao foi possivel processar a estrutura Json." )
            Return (lOk)
        endif

        If oDados['STATUS']

            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "05"
            oResponse["content"]["Type"]	:= "SUCCESS"
            oResponse["content"]["Message"]	:= oDados['PEDIDO_COMPRA']

            cResponse := EncodeUTF8(oResponse:ToJson())
            ::SetResponse(cResponse)

        Else
        
            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "A5"
            oResponse["content"]["Type"]	:= "ERRO"
            oResponse["content"]["Message"]	:= oDados['WARN']

            
            cErro :=  (EncodeUTF8(oResponse:ToJson()))
            SetRestFault(400, EncodeUTF8(cErro))
            lOk := .F.

        EndIf

    Return (lOk)

    // 6 - PRE-NOTA
    WSMETHOD POST PreNota WSREST WsPayTrac01
        Local cJson      := ::GetContent()
        Local oResponse  := JsonObject():New()
        Local oParse     := JsonObject():New()
        Local lOk        := .T.
        Local cErr

        if u_isInTrans()
            lOk := .F.
            DisarmTransaction()
            SetRestFault(500, '{"erro - Thread em Aberto"}')
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][PRENOTA][ERROR] - Thread em aberto...')
            Return (lOk)
        endIf

        // define o tipo de retorno do método
        ::SetContentType("application/json")

        cErr := oParse:fromJson(cJson)

        if ValType(cErr) == "U"
            U_pFwLog("INFO","WsPayTrac01",'[WsPayTrac01][PRENOTA][SUCCESS] - Sucesso parser Json String')
            oDados := u_PayTra14(oParse)
        else
            lOk := .F.
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][PRENOTA][ERROR] - Falha parser Json String')
            SetRestFault( 1, "Nao foi possivel processar a estrutura Json." )
            Return (lOk)
        endif

        If oDados['STATUS']

            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "06"
            oResponse["content"]["Type"]	:= "SUCCESS"
            oResponse["content"]["Message"]	:= oDados['PRE_NOTA']

            cResponse := EncodeUTF8(oResponse:ToJson())
            ::SetResponse(cResponse)

        Else
        
            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "A6"
            oResponse["content"]["Type"]	:= "ERRO"
            oResponse["content"]["Message"]	:= oDados['WARN']

            
            cErro :=  (EncodeUTF8(oResponse:ToJson()))
            SetRestFault(400, EncodeUTF8(cErro))
            lOk := .F.

        EndIf

    Return (lOk)
    
    // 7 - NOTA FISCAL
    WSMETHOD POST NfReembolso WSREST WsPayTrac01
        Local cJson      := ::GetContent()
        Local oResponse  := JsonObject():New()
        Local oParse     := JsonObject():New()
        Local lOk        := .T.
        Local cErr

        if u_isInTrans()
            lOk := .F.
            DisarmTransaction()
            SetRestFault(500, '{"erro - Thread em Aberto"}')
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][NFREEMBOLSO][ERROR] - Thread em aberto...')
            Return (lOk)
        endIf

        // define o tipo de retorno do método
        ::SetContentType("application/json")

        cErr := oParse:fromJson(cJson)

        if ValType(cErr) == "U"
            U_pFwLog("INFO","WsPayTrac01",'[WsPayTrac01][NFREEMBOLSO][SUCCESS] - Sucesso parser Json String')
            oDados := u_PayTra04(oParse)
        else
            lOk := .F.
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][NFREEMBOLSO][ERROR] - Falha parser Json String')
            SetRestFault( 1, "Nao foi possivel processar a estrutura Json." )
            Return (lOk)
        endif

        If oDados['STATUS']

            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "07"
            oResponse["content"]["Type"]	:= "SUCCESS"
            oResponse["content"]["Message"]	:= oDados['NF_REEMBOLSO']

            cResponse := EncodeUTF8(oResponse:ToJson())
            ::SetResponse(cResponse)

        Else
        
            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "A7"
            oResponse["content"]["Type"]	:= "ERRO"
            oResponse["content"]["Message"]	:= oDados['WARN']

            
            cErro :=  (EncodeUTF8(oResponse:ToJson()))
            SetRestFault(400, EncodeUTF8(cErro))
            lOk := .F.

        EndIf

    Return (lOk)


//FIM PACOTE PADRAO

// PACOTE CUSTOMIZADO
    WsMethod POST ValidaID WSREST WsPayTrac01

        Local cJson      := ::GetContent()
        Local oResponse  := JsonObject():New()
        Local oParse     := JsonObject():New()
        Local lOk        := .T.
        Local cErro      := ""
        Local cErr

        if u_isInTrans()
            lOk := .F.
            DisarmTransaction()
            SetRestFault(500, '{"erro - Thread em Aberto"}')
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][ERROR] - Thread em aberto...')
            Return (lOk)
        endIf

        // define o tipo de retorno do método
        ::SetContentType("application/json")

        cErr := oParse:fromJson(cJson)
        if ValType(cErr) == "U"
            U_pFwLog("INFO","WsPayTrac01",'[WsPayTrac01][SUCCESS] - Sucesso parser Json String')
            oDados := u_PayTra01(oParse)
        else
            lOk := .F.
            U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][ERROR] - Falha parser Json String')
            SetRestFault( 1, "Nao foi possivel processar a estrutura Json." )
            Return (lOk)
        endif

        If oDados['STATUS']

            U_pFwLog("INFO","WsPayTrac01",'[WsPayTrac01][SUCCESS] - Id disponivel')

            lOk := .T.

            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "08"
            oResponse["content"]["Type"]	:= "SUCCESS"
            oResponse["content"]["Message"]	:= oDados['VALIDAID']

            cResponse := EncodeUTF8(oResponse:ToJson())
            ::SetResponse(cResponse)

        Else

            lOk := .F.

            oResponse["content"] := JsonObject():New()
            oResponse["content"]["id"]	    := "A8"
            oResponse["content"]["Type"]	:= "ERROR"
            oResponse["content"]["Message"] := oDados['WARN']

            cErro := oResponse:ToJson()
            SetRestFault(400, EncodeUTF8(cErro))

        EndIf

    Return (lOk)

    WsMethod POST Aprovadores WSREST WsPayTrac01

            Local cJson      := ::GetContent()
            Local oResponse  := JsonObject():New()
            Local oParse     := JsonObject():New()
            Local lOk        := .T.
            Local cErr

            if u_isInTrans()
                lOk := .F.
                DisarmTransaction()
                SetRestFault(500, '{"erro - Thread em Aberto"}')
                U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][APROVADORES][ERROR] - Thread em aberto...')
                Return (lOk)
            endIf

            // define o tipo de retorno do método
            ::SetContentType("application/json")

            cErr := oParse:fromJson(cJson)

            if ValType(cErr) == "U"
                U_pFwLog("INFO","WsPayTrac01",'[WsPayTrac01][APROVADORES][SUCCESS] - Sucesso parser Json String')
                oDados := u_PayTra04(oParse)
            else
                lOk := .F.
                U_pFwLog("ERROR","WsPayTrac01",'[WsPayTrac01][APROVADORES][ERROR] - Falha parser Json String')
                SetRestFault( 1, "Nao foi possivel processar a estrutura Json." )
                Return (lOk)
            endif

            If oDados['STATUS']

                oResponse["content"] := JsonObject():New()
                oResponse["content"]["id"]	    := "09"
                oResponse["content"]["Type"]	:= "SUCCESS"
                oResponse["content"]["Message"]	:= oDados['CENTROS_CUSTO']

                cResponse := EncodeUTF8(oResponse:ToJson())
                ::SetResponse(cResponse)

            Else
            
                oResponse["content"] := JsonObject():New()
                oResponse["content"]["id"]	    := "A9"
                oResponse["content"]["Type"]	:= "ERRO"
                oResponse["content"]["Message"]	:= oDados['WARN']

                
                cErro :=  (EncodeUTF8(oResponse:ToJson()))
                SetRestFault(400, EncodeUTF8(cErro))
                lOk := .F.

            EndIf


    Return (lOk)
//FIM PACOTE CUSTOMIZADO

