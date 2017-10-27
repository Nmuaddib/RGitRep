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

select * from TS.FAIXA_ETARIA

select  distinct
        pcm.TIPO_FAIXA_ETARIA,
        pcm.COD_FAIXA_ETARIA,
        fe.DESC_FAIXA
from    ts.posicao_cadastro_mes pcm,
        TS.FAIXA_ETARIA fe
where   pcm.mes_ano_ref between to_date ('01/01/2014','dd/mm/yyyy') and to_date ('01/07/2017','dd/mm/yyyy') 
and     pcm.TIPO_FAIXA_ETARIA = fe.TIPO_FAIXA
and     pcm.COD_FAIXA_ETARIA = fe.COD_FAIXA_ETARIA    


select  to_char(pcm.mes_ano_ref,'RRRRMM') anomes,
        sum(qtd_ativos) BT
from    ts.posicao_cadastro_mes pcm           
where   pcm.mes_ano_ref between to_date('01/01/2014','dd/mm/yyyy') and to_date('01/08/2017','dd/mm/yyyy')
--and     pcm.COD_FAIXA_ETARIA = 10
group by   
        to_char(pcm.mes_ano_ref,'RRRRMM')
order by 1        



