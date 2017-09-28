select  decode(fc.IND_INTERNACAO||' - '||nvl(di.DESC_CLASSIFICACAO_DESPESA, 'NA'),
'Não - Consulta Eletiva',       'Consultas',
'Não - Diárias e Taxas',        'Diárias - Amb',
'Não - Home Care',              'NA',
'Não - Honorários',             'Honorários - Amb',
'Não - Material',               'Material - Amb',
'Não - Medicamento',            'Medicamento - Amb',
'Não - Remoção',                'Remoção - Amb',
'Não - SADT',                   'Exames - Amb',
'Não - Urgência e Emergência',  'Emergência',
'Não - Valor Referencial',      'Pacotes - Amb',
'Não - NA',                     'NA',
'Sim - Consulta Eletiva',       'NA',
'Sim - Diárias e Taxas',        'Diárias - Inter',
'Sim - Home Care',              'Domiciliar',
'Sim - Honorários',             'Honorários - Inter',
'Sim - Material',               'Material - Inter',
'Sim - Medicamento',            'Medicamento - Inter',
'Sim - Remoção',                'Remoção - Inter',
'Sim - SADT',                   'Exames - Inter',
'Sim - Urgência e Emergência',  'NA',
'Sim - Valor Referencial',      'Pacotes - Inter',
'Sim - NA',                     'NA',
'NA') classe,
        substr(fc.ID_TEMPO_MES_ANO_REF,1,4) ano,
        substr(fc.ID_TEMPO_MES_ANO_REF,1,6) anomes,
        sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0))  VT,        
--        round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(tbt.BT),2) VM, 
        round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(count(distinct fc.COD_TS)),2) VA,
        round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(count(distinct fc.NUM_PEDIDO)),2) VR,        
        round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(count(distinct fc.CONTA)),2) VC,
--        round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(count(1)),2) VI,
        sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0))  QP,        
--        round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(tbt.BT),2) QM,       
        round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(count(distinct fc.COD_TS)),2) QA,
        round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(count(distinct fc.NUM_PEDIDO)),2) QR,        
        round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(count(distinct fc.CONTA)),2) QC,
--        round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(count(1)),2) QI,
        (sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/decode(sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)), 0, 1, sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0))) VQ,
--        tbt.BT, 
        count(distinct fc.COD_TS) BA,
        count(distinct fc.NUM_PEDIDO)/count(distinct fc.COD_TS) BR,    
        count(distinct fc.CONTA) CT--,
--        count(1) IT--,
--        count(distinct fc.COD_TS)/tbt.BT PA        
from    TS.FAT_ITEM_CONTA fc,
        TS.DIM_ITEM di/*,
        (select  to_char(pcm.mes_ano_ref,'RRRRMM') anomes,
                sum(qtd_ativos) BT
        from    ts.posicao_cadastro_mes pcm           
        where   pcm.mes_ano_ref between to_date ('01/01/2014','dd/mm/yyyy') and to_date ('01/08/2017','dd/mm/yyyy')
--        and     pcm.COD_FAIXA_ETARIA = 10
        group by   
                to_char(pcm.mes_ano_ref,'RRRRMM')
        order by 1) tbt,
        (select  b.COD_TS,
                b.NOME_ASSOCIADO,
                b.DATA_NASCIMENTO,
                2017-to_number(to_char(b.DATA_NASCIMENTO,'RRRR')) idade
        from    ts.dim_beneficiario b
        where   2017-to_number(to_char(b.DATA_NASCIMENTO,'RRRR')) > 58) bnf*/
where   /*substr(fc.ID_TEMPO_MES_ANO_REF,1,6) = tbt.anomes
and     */fc.ITEM_MEDICO = di.ITEM_MEDICO
and     fc.IND_INTERNACAO = 'Sim'
and     fc.NUM_PEDIDO is not null
--and     fc.COD_TS = bnf.COD_TS
group by decode(fc.IND_INTERNACAO||' - '||nvl(di.DESC_CLASSIFICACAO_DESPESA, 'NA'),
'Não - Consulta Eletiva',       'Consultas',
'Não - Diárias e Taxas',        'Diárias - Amb',
'Não - Home Care',              'NA',
'Não - Honorários',             'Honorários - Amb',
'Não - Material',               'Material - Amb',
'Não - Medicamento',            'Medicamento - Amb',
'Não - Remoção',                'Remoção - Amb',
'Não - SADT',                   'Exames - Amb',
'Não - Urgência e Emergência',  'Emergência',
'Não - Valor Referencial',      'Pacotes - Amb',
'Não - NA',                     'NA',
'Sim - Consulta Eletiva',       'NA',
'Sim - Diárias e Taxas',        'Diárias - Inter',
'Sim - Home Care',              'Domiciliar',
'Sim - Honorários',             'Honorários - Inter',
'Sim - Material',               'Material - Inter',
'Sim - Medicamento',            'Medicamento - Inter',
'Sim - Remoção',                'Remoção - Inter',
'Sim - SADT',                   'Exames - Inter',
'Sim - Urgência e Emergência',  'NA',
'Sim - Valor Referencial',      'Pacotes - Inter',
'Sim - NA',                     'NA',
'NA'),
         substr(fc.ID_TEMPO_MES_ANO_REF,1,4),
         substr(fc.ID_TEMPO_MES_ANO_REF,1,6)--,
         --tbt.BT
having  sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)) > 0
or      sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)) > 0
order by 3, 1

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

select  substr(fc.ID_TEMPO_MES_ANO_REF,1,4) ano,
        substr(fc.ID_TEMPO_MES_ANO_REF,1,6) anomes,
        sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0))  VT,        
        round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(tbt.BT),2) VM, 
        round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(count(distinct fc.COD_TS)),2) VA,
        round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(count(distinct fc.NUM_PEDIDO)),2) VR,        
        round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(count(distinct fc.CONTA)),2) VC,
--        round((sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/(count(1)),2) VI,
        sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0))  QP,        
        round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(tbt.BT),2) QM,       
        round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(count(distinct fc.COD_TS)),2) QA,
        round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(count(distinct fc.NUM_PEDIDO)),2) QR,        
        round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(count(distinct fc.CONTA)),2) QC,
--        round((sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)))/(count(1)),2) QI,
        (sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)))/decode(sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)), 0, 1, sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0))) VQ,
        tbt.BT, 
        count(distinct fc.COD_TS) BA,
        count(distinct fc.NUM_PEDIDO)/count(distinct fc.COD_TS) BR,    
        count(distinct fc.CONTA) CT,
--        count(1) IT--,
        count(distinct fc.COD_TS)/tbt.BT PA        
from    TS.FAT_ITEM_CONTA fc,
        TS.DIM_ITEM di,
        (select  to_char(pcm.mes_ano_ref,'RRRRMM') anomes,
                sum(qtd_ativos) BT
        from    ts.posicao_cadastro_mes pcm           
        where   pcm.mes_ano_ref between to_date ('01/01/2014','dd/mm/yyyy') and to_date ('01/08/2017','dd/mm/yyyy')
--        and     pcm.COD_FAIXA_ETARIA = 10
        group by   
                to_char(pcm.mes_ano_ref,'RRRRMM')
        order by 1) tbt
        /*(select  b.COD_TS,
                b.NOME_ASSOCIADO,
                b.DATA_NASCIMENTO,
                2017-to_number(to_char(b.DATA_NASCIMENTO,'RRRR')) idade
        from    ts.dim_beneficiario b
        where   2017-to_number(to_char(b.DATA_NASCIMENTO,'RRRR')) > 58) bnf*//**/
where   substr(fc.ID_TEMPO_MES_ANO_REF,1,6) = tbt.anomes
and     fc.ITEM_MEDICO = di.ITEM_MEDICO
and     fc.IND_INTERNACAO = 'Sim'
and     fc.NUM_PEDIDO is not null
--and     fc.COD_TS = bnf.COD_TS
group by substr(fc.ID_TEMPO_MES_ANO_REF,1,4),
         substr(fc.ID_TEMPO_MES_ANO_REF,1,6),
         tbt.BT
having  sum(nvl(fc.QTD_ITEM,0)) - sum(nvl(fc.QTD_GLOSADO,0)) > 0
or      sum(nvl(fc.VAL_APROVADO_ITEM,0)) + sum(nvl(fc.VALOR_PAGO_REVISAO,0)) > 0
order by 2
