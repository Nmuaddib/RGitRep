        select  to_char(pcm.mes_ano_ref,'RRRRMM') anomes,
                
                fe.DESC_FAIXA faixa,
                sum(qtd_ativos) BT
        from    ts.posicao_cadastro_mes pcm,
                TS.FAIXA_ETARIA fe
        where   pcm.mes_ano_ref between to_date ('01/01/2014','dd/mm/yyyy') and to_date ('01/07/2017','dd/mm/yyyy') 
        and     pcm.TIPO_FAIXA_ETARIA = fe.TIPO_FAIXA
        and     pcm.COD_FAIXA_ETARIA = fe.COD_FAIXA_ETARIA
        group by   
                to_char(pcm.mes_ano_ref,'RRRRMM'),
                fe.DESC_FAIXA
        order by 1,2



