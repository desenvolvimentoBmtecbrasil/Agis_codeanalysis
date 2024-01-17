
/*/{Protheus.doc} F50PERGUNT

O ponto de entrada: F50PERGUNT 
permite alterar a configuração dos perguntes via rotina automática - ExecAuto.

@type function
@author BMTEC
@since 29/08/2023


/*/

User Function F50PERGUNT()

	//[1 - SIM | 2- NAO]

	MV_PAR04 := 1 // CONTABILIZA ONLINE
	MV_PAR05 := 2 // GERA CHEQUE P/ ADIANTAMENTO
	MV_PAR09 := 1 // MOVIMENTACAO BANCARIA SEM CHEQUE

Return Nil

/*/{Protheus.doc} F050RAUT

Permite a gravação do rateio pre-configurado na inclusão do contas a pagar via execauto

@type function
@author BMTEC
@since 29/08/2023


/*/

User Function F050RAUT()
	Local aRet := {}
	
	/*aDRatCV4 - variavel privada inicializada no fonte paytra03.prw*/
	AADD(aRet, 2)
	AADD(aRet, aDRatCV4[1,1])
	AADD(aRet, '')
	AADD(aRet, '')
	AADD(aRet, '')

Return aRet
