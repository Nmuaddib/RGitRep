select  dp.NOME_MACRO_REGIAO,
        dp.NOME_MICRO_REGIAO,
        dp.NOME_CIDADE,
        --fc.ITEM_MEDICO,
        count(distinct fc.COD_PRESTADOR_TS) prestadores,
        count(distinct fc.COD_TS) beneficiarios,         
        sum(fc.VAL_APROVADO_ITEM) VALOR,
        sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)) QP
from    TS.FAT_ITEM_CONTA fc,
        TS.DIM_PRESTADOR dp
where   fc.COD_PRESTADOR_TS = dp.COD_PRESTADOR_TS
----
and     substr(fc.ID_TEMPO_MES_ANO_REF,1,6) between '201703' and '201803'
and     fc.IND_INTERNACAO = 'Não'
and     (fc.ITEM_MEDICO between 33000000 and 34999999
or      fc.ITEM_MEDICO between 36000000 and 36999999
or      fc.ITEM_MEDICO between 32010000 and 32090000
or      fc.ITEM_MEDICO = 32110014)
group by dp.NOME_MACRO_REGIAO,
         dp.NOME_MICRO_REGIAO,
         dp.NOME_CIDADE--,
         --fc.ITEM_MEDICO
         
select  dp.NOME_MACRO_REGIAO,
        dp.NOME_MICRO_REGIAO,
        dp.NOME_CIDADE,
        --fc.ITEM_MEDICO,
        fc.COD_PRESTADOR_TS,
        dp.NOME_PRESTADOR,
        count(distinct fc.COD_TS) beneficiarios,         
        sum(fc.VAL_APROVADO_ITEM) VALOR,
        sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)) QP
from    TS.FAT_ITEM_CONTA fc,
        TS.DIM_PRESTADOR dp
where   fc.COD_PRESTADOR_TS = dp.COD_PRESTADOR_TS
----
and     substr(fc.ID_TEMPO_MES_ANO_REF,1,6) between '201703' and '201803'
and     fc.IND_INTERNACAO = 'Não'
and     (fc.ITEM_MEDICO between 33000000 and 34999999
or      fc.ITEM_MEDICO between 36000000 and 36999999
or      fc.ITEM_MEDICO between 32010000 and 32090000
or      fc.ITEM_MEDICO = 32110014)
group by dp.NOME_MACRO_REGIAO,
         dp.NOME_MICRO_REGIAO,
         dp.NOME_CIDADE,
         --fc.ITEM_MEDICO   
         fc.COD_PRESTADOR_TS,
         dp.NOME_PRESTADOR       

desc TS.FAT_ITEM_CONTA
desc TS.DIM_PRESTADOR
desc TS.DIM_ITEM