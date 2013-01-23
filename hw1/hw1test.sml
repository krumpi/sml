
val test_is_older_1=is_older((2011, 12, 3), (2011, 12, 4))=true
val test_is_older_2=is_older((2011, 12, 3), (2011, 12, 2))=false
val test_is_older_3=is_older((2011, 11, 3), (2011, 12, 3))=true
val test_is_older_4=is_older((2011, 12, 3), (2011, 11, 3))=false
val test_is_older_5=is_older((2010, 12, 3), (2011, 12, 3))=true
val test_is_older_6=is_older((2011, 12, 3), (2010, 12, 3))=false
val test_is_older_7=is_older((2011, 12, 3), (2011, 12, 3))=false
val test_is_older_8=is_older((~2011, 12, 3), (~2011, 11, 3))=false

val test_number_in_month_1=number_in_month([], 1)=0
val test_number_in_month_2=number_in_month([(2011,1,1)], 1)=1
val test_number_in_month_3=number_in_month([(2011,1,1), (2011,1,3)], 1)=2
val test_number_in_month_4=number_in_month([(2011,1,1), (2011,1,3), (2011,4,1)], 1)=2
val test_number_in_month_5=number_in_month([(2011,1,1), (2011,1,3), (2011,4,1)], 4)=1
val test_number_in_month_6=number_in_month([(2011,4,1), (2011,1,3), (2011,4,1)], 4)=2
val test_number_in_month_7=number_in_month([(2011,8,1), (2011,1,3), (2011,4,1)], 4)=1

val test_number_in_months_0=number_in_months([], [])=0
val test_number_in_months_1=number_in_months([], [1])=0
val test_number_in_months_2=number_in_months([(2011,1,1)], [1])=1
val test_number_in_months_3=number_in_months([(2011,1,1), (2011,1,3)], [1])=2
val test_number_in_months_4=number_in_months([(2011,1,1), (2011,1,3), (2011,4,1)], [1])=2
val test_number_in_months_5=number_in_months([(2011,1,1), (2011,1,3), (2011,4,1)], [1, 4])=3
val test_number_in_months_6=number_in_months([(2011,4,1), (2011,1,3), (2011,4,1)], [4])=2
val test_number_in_months_7=number_in_months([(2011,8,1), (2011,1,3), (2011,4,1)], [4, 8, 1])=3

val test_date_in_month_1=dates_in_month([(2011,8,1), (2011,1,3), (2011,4,1)], [])=[]