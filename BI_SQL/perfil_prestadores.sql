select  dp.COD_PRESTADOR_TS codigo,
        dp.NOME_PRESTADOR nome,
        substr(fc.ID_TEMPO_MES_ANO_REF,1,6) anomes,
        count(distinct fc.COD_TS) bn,
        sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)) quantidade,
        sum(nvl(fc.VAL_APROVADO_ITEM,0)) valor,
        sum(nvl(fc.VALOR_PAGO_REVISAO,0)) revisao
from    TS.FAT_ITEM_CONTA fc,
        TS.DIM_PRESTADOR dp/*,
        (select * from
            (select fc.COD_PRESTADOR_TS codigo,
                    sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)) VA_FT
            from    TS.FAT_ITEM_CONTA fc
            where   substr(fc.ID_TEMPO_MES_ANO_REF,1,4) = '2017'
            group by fc.COD_PRESTADOR_TS
            order by 2 desc) t
        where rownum <= 200) tp*/        
where   fc.COD_PRESTADOR_TS(+) = dp.COD_PRESTADOR_TS
and     substr(fc.ID_TEMPO_MES_ANO_REF,1,6) between '201601' and '201708'
--and     dp.COD_PRESTADOR_TS = tp.codigo
and     dp.NOME_PRESTADOR like '%HOSPITAL SANTA IZABEL%'
group by dp.COD_PRESTADOR_TS,
         dp.NOME_PRESTADOR,
         substr(fc.ID_TEMPO_MES_ANO_REF,1,6)
order by 1,3
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
/*
select  hp.cod_especialidade, 
        EA.NOME_ESPECIALIDADE_AMB nome_especialidade, 
        HP.DATA_INI_HAB, 
        hpe.COD_EDITAL  
from    ts.habilitacao_prestador hp, 
        ts.prestador_servico ps, 
        TS.ESPECIALIDADE_AMB ea, 
        TS.HABILITACAO_PRESTADOR_EDITAL hpe
where   hp.cod_prestador_ts = '594480'
and     hp.cod_prestador_ts = ps.cod_prestador_ts
and     hp.item_medico = EA.COD_ESPECIALIDADE_AMB
and     hp.cod_especialidade = hpe.cod_especialidade
and     hp.cod_prestador_ts = hpe.cod_prestador_ts
order by 2
*/

select  hp.cod_especialidade, 
        esp.NOME_ESPECIALIDADE, 
        to_date(HP.DATA_INI_HAB,'dd/mm/rrrr') data_ini_hab, 
        hpe.COD_EDITAL  
from    ts.habilitacao_prestador hp, 
        ts.prestador_servico ps, 
        TS.ESPECIALIDADE esp, 
        TS.HABILITACAO_PRESTADOR_EDITAL hpe
where   hp.cod_prestador_ts = '594480'
and     hp.cod_prestador_ts = ps.cod_prestador_ts
and     hp.COD_ESPECIALIDADE = esp.COD_ESPECIALIDADE
and     hp.cod_especialidade = hpe.cod_especialidade
and     hp.cod_prestador_ts = hpe.cod_prestador_ts
order by 2
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
select  dp.COD_PRESTADOR_TS codigo,
        dp.NOME_PRESTADOR nome,
        fc.NOME_TIPO_TRATAMENTO tratamento,
        substr(fc.ID_TEMPO_MES_ANO_REF,1,6) anomes,
        count(distinct fc.COD_TS) bn,
        sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)) quantidade,
        sum(nvl(fc.VAL_APROVADO_ITEM,0)) valor,
        sum(nvl(fc.VALOR_PAGO_REVISAO,0)) revisao
from    TS.FAT_ITEM_CONTA fc,
        TS.DIM_PRESTADOR dp/*,
        (select * from
            (select fc.COD_PRESTADOR_TS codigo,
                    sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)) VA_FT
            from    TS.FAT_ITEM_CONTA fc
            where   substr(fc.ID_TEMPO_MES_ANO_REF,1,4) = '2017'
            group by fc.COD_PRESTADOR_TS
            order by 2 desc) t
        where rownum <= 200) tp*/        
where   fc.COD_PRESTADOR_TS(+) = dp.COD_PRESTADOR_TS
and     substr(fc.ID_TEMPO_MES_ANO_REF,1,6) between '201601' and '201708'
--and     dp.COD_PRESTADOR_TS = tp.codigo
and     dp.NOME_PRESTADOR like '%HOSPITAL SANTA IZABEL%'
group by dp.COD_PRESTADOR_TS,
         dp.NOME_PRESTADOR,
         fc.NOME_TIPO_TRATAMENTO,
         substr(fc.ID_TEMPO_MES_ANO_REF,1,6)
order by 1,3,4
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
select  dp.COD_PRESTADOR_TS codigo,
        dp.NOME_PRESTADOR nome,
        di.NOME_ESPECIALIDADE especialidade,
        substr(fc.ID_TEMPO_MES_ANO_REF,1,6) anomes,
        count(distinct fc.COD_TS) bn,
        sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)) quantidade,
        sum(nvl(fc.VAL_APROVADO_ITEM,0)) valor,
        sum(nvl(fc.VALOR_PAGO_REVISAO,0)) revisao
from    TS.FAT_ITEM_CONTA fc,
        TS.DIM_ITEM di,
        TS.DIM_PRESTADOR dp/*,
        (select * from
            (select fc.COD_PRESTADOR_TS codigo,
                    sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)) VA_FT
            from    TS.FAT_ITEM_CONTA fc
            where   substr(fc.ID_TEMPO_MES_ANO_REF,1,4) = '2017'
            group by fc.COD_PRESTADOR_TS
            order by 2 desc) t
        where rownum <= 200) tp*/        
where   fc.COD_PRESTADOR_TS(+) = dp.COD_PRESTADOR_TS
and     fc.ITEM_MEDICO = di.ITEM_MEDICO
and     substr(fc.ID_TEMPO_MES_ANO_REF,1,6) between '201601' and '201708'
--and     dp.COD_PRESTADOR_TS = tp.codigo
and     dp.NOME_PRESTADOR like '%HOSPITAL SANTA IZABEL%'
group by dp.COD_PRESTADOR_TS,
         dp.NOME_PRESTADOR,
         di.NOME_ESPECIALIDADE,
         substr(fc.ID_TEMPO_MES_ANO_REF,1,6)
order by 1,3,4
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
select  dp.COD_PRESTADOR_TS codigo,
        dp.NOME_PRESTADOR nome,
        di.NOME_ESPECIALIDADE especialidade,
        di.NOME_ITEM item,
        di.NOME_GRUPO_ESTATISTICO grupo,
        substr(fc.ID_TEMPO_MES_ANO_REF,1,6) anomes,
        count(distinct fc.COD_TS) bn,
        sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)) quantidade,
        sum(nvl(fc.VAL_APROVADO_ITEM,0)) valor,
        sum(nvl(fc.VALOR_PAGO_REVISAO,0)) revisao
from    TS.FAT_ITEM_CONTA fc,
        TS.DIM_ITEM di,
        TS.DIM_PRESTADOR dp/*,
        (select * from
            (select fc.COD_PRESTADOR_TS codigo,
                    sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)) VA_FT
            from    TS.FAT_ITEM_CONTA fc
            where   substr(fc.ID_TEMPO_MES_ANO_REF,1,4) = '2017'
            group by fc.COD_PRESTADOR_TS
            order by 2 desc) t
        where rownum <= 200) tp*/        
where   fc.COD_PRESTADOR_TS(+) = dp.COD_PRESTADOR_TS
and     fc.ITEM_MEDICO = di.ITEM_MEDICO
and     substr(fc.ID_TEMPO_MES_ANO_REF,1,6) between '201601' and '201708'
--and     dp.COD_PRESTADOR_TS = tp.codigo
and     dp.NOME_PRESTADOR like '%HOSPITAL SANTA IZABEL%'
group by dp.COD_PRESTADOR_TS,
         dp.NOME_PRESTADOR,
         di.NOME_ESPECIALIDADE,
         di.NOME_ITEM,         
         di.NOME_GRUPO_ESTATISTICO,
         substr(fc.ID_TEMPO_MES_ANO_REF,1,6)
order by 1,3,4,5,6
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
select  dp.COD_PRESTADOR_TS codigo,
        dp.NOME_PRESTADOR nome,
        di.NOME_ITEM item,
        substr(fc.ID_TEMPO_MES_ANO_REF,1,6) anomes,
        count(distinct fc.COD_TS) bn,
        sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)) quantidade,
        sum(nvl(fc.VAL_APROVADO_ITEM,0)) valor,
        sum(nvl(fc.VALOR_PAGO_REVISAO,0)) revisao
from    TS.FAT_ITEM_CONTA fc,
        TS.DIM_ITEM di,
        TS.DIM_PRESTADOR dp/*,
        (select * from
            (select fc.COD_PRESTADOR_TS codigo,
                    sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)) VA_FT
            from    TS.FAT_ITEM_CONTA fc
            where   substr(fc.ID_TEMPO_MES_ANO_REF,1,4) = '2017'
            group by fc.COD_PRESTADOR_TS
            order by 2 desc) t
        where rownum <= 200) tp*/        
where   fc.COD_PRESTADOR_TS(+) = dp.COD_PRESTADOR_TS
and     fc.ITEM_MEDICO = di.ITEM_MEDICO
and     substr(fc.ID_TEMPO_MES_ANO_REF,1,6) between '201601' and '201708'
and     di.NOME_ITEM like '%Provas De Pro%'
--and     dp.COD_PRESTADOR_TS = tp.codigo
and     dp.NOME_PRESTADOR like '%HOSPITAL SANTA IZABEL%'
group by dp.COD_PRESTADOR_TS,
         dp.NOME_PRESTADOR,
         di.NOME_ITEM,
         substr(fc.ID_TEMPO_MES_ANO_REF,1,6)
order by 1,3,4

SELECT * FROM V$NLS_PARAMETERS
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
select er.mes_ano_ref, 
       case when er.mes_ano_ref <= '01/06/2017' then
           x1.QTD_APS
       else
           x.valor_orcamento
       end valor_cota,   
       (ER.VAL_INFORMADO - ER.VAL_GLOSADO - ER.VAL_GLOSADO_EDITAL) val_pago 
from   ts.extrato_remessa er, 
       (SELECT pco.mes_ano_ref, 
               pco.COD_PRESTADOR_TS, 
               pco.VALOR_ORCAMENTO 
        FROM    ts.PRESTADOR_COTA_ORCAMENTARIA pco) x, 
       (SELECT COD_PRESTADOR_TS, 
               SUM(QTD_APS) qtd_aps, 
               DATA_INICIO_APS 
        FROM   (SELECT  DATA_INICIO_APS, 
                        COD_PRESTADOR_TS, 
                        QTD_APS 
                FROM    TS.PRESTADOR_APS_HOSPITALAR 
                UNION 
                SELECT  pa.DATA_INICIO_APS, 
                        pa.COD_PRESTADOR_TS, 
                        (pa.QTD_APS*nvl(CP.VAL_PROCEDIMENTO,0)) QTD_APS 
                FROM    TS.PRESTADOR_APS pa, 
                        ts.classificacao_preco cp 
                WHERE PA.ID_CLASSIFICACAO = CP.ID_CLASSIFICACAO) 
        GROUP BY COD_PRESTADOR_TS, 
                 DATA_INICIO_APS) x1 
where   er.mes_ano_ref between '01/01/2016' and '01/08/2017'
and     er.cod_prestador_ts = '594480'
and     er.mes_ano_ref = x.mes_ano_ref(+)
and     er.cod_prestador_ts = x.cod_prestador_ts(+)
and     er.mes_ano_ref = x1.DATA_INICIO_APS(+)
and     er.cod_prestador_ts = x1.cod_prestador_ts(+)
order by 1