
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

val test_date_in_month_1=dates_in_month([], 1)=[]
val test_date_in_month_2=dates_in_month([(2011,8,1), (2011,1,3), (2011,4,1)], 1)=[(2011,1,3)]
val test_date_in_month_3=dates_in_month([(2011,8,1), (2011,1,3), (2011,4,1)], 5)=[]
val test_date_in_month_4=dates_in_month([(2011,4,1), (2011,1,3), (2011,4,1)], 4)=[(2011,4,1), (2011,4,1)]

val test_dates_in_month_1=dates_in_months([], [])=[]
val test_dates_in_month_2=dates_in_months([], [1])=[]
val test_dates_in_month_3=dates_in_months([(2011,8,1), (2011,1,3), (2011,4,1)], [1])=[(2011,1,3)]
val test_dates_in_month_4=dates_in_months([(2011,8,1), (2011,1,3), (2011,4,1)], [5])=[]
val test_dates_in_month_5=dates_in_months([(2011,4,1), (2011,1,3), (2011,4,8)], [4, 1])=[(2011,4,1), (2011,4,8), (2011,1,3)]

val test_get_nth_1=get_nth(["abc"], 1)="abc"
val test_get_nth_2=get_nth(["abc", "dc"], 1)="abc"
val test_get_nth_3=get_nth(["abc", "dc"], 2)="dc"
val test_get_nth_4=get_nth(["dc", "ef", "gh"], 3)="gh"

val test_date_to_string_1=date_to_string((2011, 8, 7))="August 7, 2011"
val test_date_to_string_2=date_to_string((~2011, 8, 7))="August 7, ~2011"
val test_date_to_string_3=date_to_string((1973, 7, 9))="July 9, 1973"

val test_number_before_reaching_sum_1=number_before_reaching_sum(4, [5,6,1,2]) = 0
val test_number_before_reaching_sum_2=number_before_reaching_sum(5, [5,6,1,2]) = 0
val test_number_before_reaching_sum_3=number_before_reaching_sum(13,[5,6,1,2]) = 3
val test_number_before_reaching_sum_4=number_before_reaching_sum(12,[5,6,1,2]) = 2

val test_what_month_1=what_month(1) = 1
val test_what_month_2=what_month(31) = 1
val test_what_month_3=what_month(150) = 5
val test_what_month_4=what_month(365) = 12


val test_month_range_1=month_range(29,34)=[1,1,1,2,2,2];
val test_month_range_2=month_range(20,19)=[];
val test_month_range_3=month_range(101,101)=[4];
val test_month_range_4=month_range(304,305)=[10,11];

 val d1 = (1983,5,28);
    val d2 = (1983,9,12);
    val d3 = (1983,9,10);
    val d4 = (1980,10,1);
    val ds = [d1,d2,d3,d4];
    val ms1 = [5,9];
    val ms2 = [9,11,6];
    val ms3 = [1,2,3,4,6,7,8];
    val ms4 = [10,5];

    val check11 = is_older(d1,d2)=true;
    val check12 = is_older(d2,d1)=false;
    val check13 = is_older(d1,d1)=false;
    val check14 = is_older(d3,d2)=true;
    val check15 = is_older(d2,d3)=false;
    val check16 = is_older(d4,d1)=true;

    val check21 = number_in_month(ds,5)=1;
    val check22 = number_in_month(ds,9)=2;
    val check23 = number_in_month(ds,7)=0;

    val check31 = number_in_months(ds,ms1)=3;
    val check32 = number_in_months(ds,ms2)=2;
    val check33 = number_in_months(ds,ms3)=0;

    val check41 = dates_in_month(ds,5)=[d1];
    val check42 = dates_in_month(ds,9)=[d2,d3];
    val check43 = dates_in_month(ds,7)=[];

    val check51 = dates_in_months(ds,ms1)=[d1,d2,d3];
    val check52 = dates_in_months(ds,ms2)=[d2,d3];
    val check53 = dates_in_months(ds,ms3)=[];
    val check54 = dates_in_months(ds,ms4)=[d4,d1];

    val ss1 = ["one","two","three","four","five"];
    val check61 = get_nth(ss1,2)="two";
    val check62 = get_nth(ss1,1)="one";

    val check71 = date_to_string(d1)="May 28, 1983";
    val check72 = date_to_string(d2)="September 12, 1983";
    val check73 = date_to_string(d3)="September 10, 1983";
    val check74 = date_to_string(d4)="October 1, 1980";

    val numbers = [1, 6, 9, 4, 2, 19];
    val check81 = number_before_reaching_sum (8, numbers)=2;
    val check82 = number_before_reaching_sum (17, numbers)=3;
    val check83 = number_before_reaching_sum (21, numbers)=4;
    val check84 = number_before_reaching_sum (1, numbers)=0;

    val check91 = what_month (10)=1;
    val check92 = what_month (360)=12;
    val check93 = what_month (150)=5;
    val check94 = what_month (290)=10;
    val check95 = what_month (60)=3;

    val check101= month_range(29,34)=[1,1,1,2,2,2];
    val check102= month_range(20,19)=[];
    val check103= month_range(101,101)=[4];
    val check104= month_range(304,305)=[10,11];

    val check111= oldest([])=NONE;
    val check112= oldest([d2])=SOME(d2);
    val check113= oldest(ds)=SOME(d4);

val a = ds
val b = ms1
val c=remove_duplicates(ds, [])

    val check121= number_in_months_challenge(ds,ms1)=number_in_months(ds,ms1);
    val check122= number_in_months_challenge(ds,ms2)=number_in_months(ds,ms2);
    val check123= number_in_months_challenge(ds,ms3)=number_in_months(ds,ms3);
    val check124= number_in_months_challenge(ds,ms1@ms1)=number_in_months(ds,ms1);
    val check125= number_in_months_challenge(ds,ms2@ms2)=number_in_months(ds,ms2);
    val check126= number_in_months_challenge(ds,ms3@ms3)=number_in_months(ds,ms3);

    (*val check127= dates_in_months_challenge(ds,ms1)=dates_in_months(ds,ms1);
    val check128= dates_in_months_challenge(ds,ms2)=dates_in_months(ds,ms2);
    val check129= dates_in_months_challenge(ds,ms3)=dates_in_months(ds,ms3);
    val check12A= dates_in_months_challenge(ds,ms4)=dates_in_months(ds,ms4);
    val check12B= dates_in_months_challenge(ds,ms1@ms1)=dates_in_months(ds,ms1);
    val check12C= dates_in_months_challenge(ds,ms2@ms2)=dates_in_months(ds,ms2);
    val check12D= dates_in_months_challenge(ds,ms3@ms3)=dates_in_months(ds,ms3);
    val check12E= dates_in_months_challenge(ds,ms4@ms4)=dates_in_months(ds,ms4);

    val check133= reasonable_date(d1)=true;
    val check134= reasonable_date(d2)=true;
    val check135= reasonable_date(d3)=true;
    val check136= reasonable_date(d4)=true;
    val check137= reasonable_date((~10,1,1))=false;
    val check138= reasonable_date((1900,2,29))=false;
    val check139= reasonable_date((1904,2,29))=true;*)