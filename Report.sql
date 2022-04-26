SELECT
	p.[Is Order Finalized],
	s.[Sale Key],
	s.[Invoice Date Key],
	s.[Delivery Date Key],
	c.[Sales Territory],
	e.Employee, 
	cust.[WWI Customer ID],
	cust.Category,
	s.[Stock Item Key],
	s.Quantity,
	s.[Unit Price],
	s.[Total Excluding Tax],
	s.[Tax Rate],
	s.Profit,
	(s.[Total Excluding Tax]-s.[Profit])/s.[Quantity] AS "Calc Unit Cost",
	sup.Supplier

FROM 
	Fact.Sale s
JOIN 
	Dimension.City c ON s.[City Key] = c.[City Key]
JOIN 
	Dimension.Employee e ON s.[Salesperson Key] = e.[Employee Key]
JOIN 
	Dimension.Customer cust ON s.[Customer Key] = cust.[Customer Key]
JOIN 
	Fact.Movement m ON s.[Stock Item Key] = m.[Stock Item Key]
JOIN 
	Dimension.Supplier sup ON m.[Supplier Key] = sup.[Supplier Key]
JOIN 
	Fact.Purchase p ON s.[Delivery Date Key] = p.[Date Key]

GROUP BY 
	p.[Is Order Finalized],
	s.[Sale Key],
	s.[Invoice Date Key],
	s.[Delivery Date Key],
	c.[Sales Territory],
	e.Employee, 
	cust.[WWI Customer ID],
	cust.Category,
	s.[Stock Item Key],
	s.Quantity,
	s.[Unit Price],
	s.[Total Excluding Tax],
	s.[Tax Rate],
	s.Profit,
	sup.Supplier
