/* Total forecasting */
select
	YEAR(jj_total.order_date) as year,
	MONTH(jj_total.order_date) as month,
	sum(jj_total.item_price_trx_cur) as total_sales
from
	jj_total
group by
	YEAR(jj_total.order_date),
	MONTH(jj_total.order_date)
order BY
	YEAR(jj_total.order_date) desc,
	MONTH(jj_total.order_date) desc;
	
-----------------------------

/* Product forecasting */
select
	jj_total.product_id,
	YEAR(jj_total.order_date) as year,
	MONTH(jj_total.order_date) as month,
	sum(jj_total.item_price_trx_cur) as total_sales
from
	jj_total

WHERE
	jj_total.product_id IN (
	'W525                     ',
	'W9335                    ',
	'W8522                    ',
	'V489H                    ',
	'V4930H                   '
	)
group by
	YEAR(jj_total.order_date),
	MONTH(jj_total.order_date),
	jj_total.product_id
order BY
	jj_total.product_id desc,
	YEAR(jj_total.order_date) desc,
	MONTH(jj_total.order_date) desc;

-----------------------------

/* Customer forecasting */
select
	jj_total.customer_id,
	jj_total.order_date,
	sum( jj_total.item_price_trx_cur )
from
	jj_total
group by
	jj_total.order_date,
	jj_total.customer_id
order by
	jj_total.order_date desc;
	
------------------------------
