select  db.NUM_ASSOCIADO COD,
        to_char(fc.DATA_ATENDIMENTO,'dd/mm/rrrr') DTA,
        di.DESC_CLASSIFICACAO_DESPESA CA,
        di.NOME_ESPECIALIDADE ESPEC,
        di.NOME_ITEM ITEM,
        di.TIPO_ITEM TIPO,
        sum(fc.VAL_APROVADO_ITEM) VALOR,
        sum(fc.QTD_ITEM) QTI,
        sum(fc.QTD_GLOSADO) QTG
from    TS.FAT_ITEM_CONTA fc,
        TS.DIM_BENEFICIARIO db,
        TS.DIM_ITEM di,
        TS.DIM_TEMPO dt,
        TS.DIM_PRESTADOR dp
where   fc.COD_TS = db.COD_TS
and     fc.ITEM_MEDICO = di.ITEM_MEDICO
and     fc.ID_TEMPO_MES_ANO_REF = dt.ID_TEMPO
and     fc.COD_PRESTADOR_TS = dp.COD_PRESTADOR_TS
----
and     dt.ANO = '2016'
and     di.TIPO_ITEM in ('I','P')
and     fc.IND_INTERNACAO = 'Não'
and     fc.NUM_PEDIDO is null
group by
        db.NUM_ASSOCIADO,
        to_char(fc.DATA_ATENDIMENTO,'dd/mm/rrrr'),
        di.DESC_CLASSIFICACAO_DESPESA,
        di.NOME_ESPECIALIDADE,
        di.NOME_ITEM,
        di.TIPO_ITEM
having  sum(fc.QTD_ITEM)-sum(fc.QTD_GLOSADO) >= 2;

desc TS.FAT_ITEM_CONTA
