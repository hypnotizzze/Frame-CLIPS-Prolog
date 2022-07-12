(deftemplate Student
 (slot Name
   (type STRING))
 (slot Name_S
   (type STRING))
 (slot Surname
   (type STRING))
 (slot SurSurname
   (type STRING))
 (slot Group
   (type INTEGER))
 (slot Course
   (type INTEGER))
)

(deftemplate Programm
 (slot Name
   (type STRING))
 (slot Name_L
   (type STRING))
 (slot Standart
   (type STRING))
 (slot Size
   (type FLOAT))
 (slot Long
   (type INTEGER))
)

(deffunction translate_name (?name)
    (if (= (str-compare ?name "Name") 0)
        then
        (return Name)
    )
    else (if (= (str-compare ?name "Name_L") 0)
        then
        (return Name_L)
    )
    else (if (= (str-compare ?name "Name_S") 0)
        then
        (return Name_S)
    )
    else (if (= (str-compare ?name "Surname") 0)
        then
        (return Surname)
    )
    else (if (= (str-compare ?name "SurSurname") 0)
        then
        (return SurSurname)
    )
    else (if (= (str-compare ?name "Group") 0)
        then
        (return Group)
    )
    else (if (= (str-compare ?name "Course") 0)
        then
        (return Course)
    )
    else (if (= (str-compare ?name "Standart") 0)
        then
        (return Standart)
    )
    else (if (= (str-compare ?name "Size") 0)
        then
        (return Size)
    )
    else (if (= (str-compare ?name "Long") 0)
        then
        (return Long)
    )
    else (return Null)
)

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction get_value(?question)
  (printout t ?question)
  (bind ?answer (read))
  (return ?answer))

(deffunction get_check_string(?question)
  (bind ?value (get_value ?question))
     (while (not (stringp ?value)) do
        (bind ?value (get_value ?question))
  )
  (return ?value)
)

(deffunction Y_or_N (?question)
    (bind ?response (ask-question ?question yes no y n да Да Нет нет))
    (if (or (or (eq ?response yes) (eq ?response y)) (or (eq ?response Да) (eq ?response да)))
        then TRUE 
        else FALSE)
)

(deffunction SearchFrame(?name)
    (do-for-all-facts ((?p1 Student)) TRUE
        (if (= (str-compare ?p1:Name ?name) 0) 
        then
            (return TRUE)
        )
    )
    (return FALSE)
)

(deffunction add_frame ()
 (bind ?name_frame (get_check_string "Введите имя фрейма: "))
 (if (Y_or_N "Добавить фрейм Студент, иначе фрейм Программа? ")
       then
           (bind ?name (get_check_string "Введите имя студента: "))
           (bind ?surname (get_check_string "Введите фамилию студента: "))
           (bind ?sursurname (get_check_string "Введите отчество студента: "))
           (bind ?group (get_value "Введите номер группы: "))
           (bind ?cource (get_value "Введите номер курса: "))
           (assert (Student (Name ?name_frame) (Name_S ?name) (Surname ?surname) (SurSurname ?sursurname) (Group ?group) (Course ?cource) ))
       else 
           (bind ?name (get_check_string "Введите название языка: "))
           (bind ?standart (get_check_string "Введите стандарт языка: "))
           (bind ?size (get_value "Введите размер программы (Мб): "))
           (bind ?long (get_value "Введите количество строк кода: "))
           (assert (Programm (Name ?name_frame) (Name_L ?name) (Standart ?standart) (Size ?size) (Long ?long) ))
 )
)

(deffunction delete_frame ()
    (facts)
    (bind ?choice (get_value "Введите номер фрейма, который хотите удалить: "))
    (while (not (integerp ?choice)) do
        (bind ?choice (get_value "Введите номер фрейма, который хотите удалить: "))
    )
    (retract ?choice)
)

(deffunction edit_frame ()
    (facts)
    (bind ?choice (get_value "Введите номер фрейма, который хотите изменить: "))
    (while (not (integerp ?choice)) do
        (bind ?choice (get_value "Введите номер фрейма, который хотите изменить: "))
    )
    (if (Y_or_N "Изменить имя фрейма (да/нет)? ")
        then
            (bind ?fr_name (get_check_string "Введите новое имя фрейма: "))
            (modify ?choice (Name ?fr_name) )

    )
)

(deffunction print_frames ()
    (facts)
)

(deffunction search_1_frame ()
 
  (if (Y_or_N "Поиск по фрейму Студент, иначе поиск по фрейму Программа(да/нет)? ")
       then
           (bind ?name (get_check_string "Введите имя студента: "))
           (bind ?surname (get_check_string "Введите фамилию студента: "))
           (bind ?sursurname (get_check_string "Введите отчество студента: "))
           (bind ?group (get_value "Введите номер группы: "))
           (bind ?course (get_value "Введите номер курса: "))
	   (bind ?count 0)
	   
           (do-for-all-facts 
 		(
   		(?p1 Student) 
 		)
                
 		(and 
		  (= (str-compare ?p1:Name_S ?name) 0)
		  (= (str-compare ?p1:Surname ?surname) 0)
		  (= (str-compare ?p1:SurSurname ?sursurname) 0)
		  (eq ?p1:Group ?group)
		  (eq ?p1:Course ?course)
		)

 		(printout t ?p1:Name crlf)
		(bind ?count 1)              
	    )
	    
	    (if (= ?count 0) then
 		(printout t "Такого фрейма нет!" crlf)) 
       else
           (bind ?name (get_check_string "Введите название языка: "))
           (bind ?standart (get_check_string "Введите стандарт языка: "))
           (bind ?size (get_value "Введите размер программы (Мб): "))
           (bind ?long (get_value "Введите количество строк кода: "))
	   (bind ?count 0)   
	
            (do-for-all-facts 
 		(
   		(?p1 Programm) 
 		)

	        (and 
 		  (= (str-compare ?p1:Name_L ?name) 0)
		  (= (str-compare ?p1:Standart ?standart) 0)
		  (eq ?p1:Size ?size)
		  (eq ?p1:Long ?long)
		)

                (printout t ?p1:Name crlf)
		(bind ?count 1)
	    )
            
	    (if (= ?count 0) then
 		(printout t "Такого фрейма нет!" crlf))
 )  
)


(deffunction search_2_frame ()
  (bind ?size (get_value "Введите количество слотов: "))
    (while (not (integerp ?size)) do
       (bind ?size (get_value "Введите количество слотов: "))
  )
  (bind ?data (create$))
  (loop-for-count (?index 1 ?size) do
     (printout t "Слот № = "?index crlf)
     (bind ?choice (ask-question "Выберите тип атрибута(1 - string, 2 - float или integer): "1 2))
     (switch ?choice 
       (case 1 then
         (bind ?data (create$ ?data (get_check_string "Введите значение атрибута у слота: ")))
       )
       (case 2 then
         (bind ?size (get_value "Введите значение атрибута у слота: "))
         (bind ?data (create$ ?size))
       )
     )
  )
	
  (bind ?count 0) 
  
  (do-for-all-facts 
   	(
   	(?p1 Student) 
 	)
                
 	TRUE
        
        (bind ?temp_first_type (create$ ?p1:Name_S ?p1:Surname ?p1:SurSurname ?p1:Group ?p1:Course))
        (bind ?na (create$ ?p1:Name))
        (if (subsetp ?data ?temp_first_type) then
          (printout t ?na " " ?temp_first_type crlf)
	  (bind ?count 1)
        )              
   )
   (do-for-all-facts 
   	(
   	(?p2 Programm) 
 	)
                
 	TRUE
        
        (bind ?temp_second_type (create$ ?p2:Name_L ?p2:Standart ?p2:Size ?p2:Long))
        (bind ?na (create$ ?p2:Name))
        (if (subsetp ?data ?temp_second_type) then
          (printout t ?na " " ?temp_second_type crlf)
	  (bind ?count 1)
        )              
   )
	  
  (if (= ?count 0) then
    (printout t "Такого фрейма нет!" crlf))
)

(deffunction search_3_frame ()
    (bind ?size (get_value "Введите количество слотов: "))
    (while (not (integerp ?size)) do
        (bind ?size (get_value "Введите количество слотов: "))
    )

    (bind ?data (create$))
    (loop-for-count (?index 1 ?size) do
        (printout t "Слот № = "?index crlf)
        (bind ?data (create$ ?data (translate_name(get_check_string "Введите название слота: "))))
    )

    (bind ?slotNamesFirstType (deftemplate-slot-names Student)) 
    (bind ?slotNamesSecondType (deftemplate-slot-names Programm))
    (delete$ ?slotNamesFirstType 1 1)
    (delete$ ?slotNamesSecondType 1 1)
    (bind ?count 0)   
	
    (do-for-all-facts ((?p1 Student))
        (and (subsetp ?slotNamesFirstType ?data) (subsetp ?data ?slotNamesFirstType))
        (printout t ?p1:Name crlf)
        (bind ?count 1)
    )

    (do-for-all-facts ((?p2 Programm))
        (and (subsetp ?slotNamesSecondType ?data) (subsetp ?data ?slotNamesSecondType))
        (printout t ?p2:Name crlf)
        (bind ?count 1)
    )

    (if (= ?count 0) then
        (printout t "Такого фрейма нет!" crlf))
    (printout t " " crlf)
)

(deffunction search_4_frame ()
    (bind ?size (get_value "Введите количество слотов: "))
    (while (not (integerp ?size)) do
        (bind ?size (get_value "Введите количество слотов: "))
    )

    (bind ?data (create$))
    (loop-for-count (?index 1 ?size) do
        (printout t "Слот № = "?index crlf)
        (bind ?data (create$ ?data (translate_name(get_check_string "Введите название слота: "))))
    )

    (bind ?slotNamesFirstType (deftemplate-slot-names Student)) 
    (bind ?slotNamesSecondType (deftemplate-slot-names Programm))
   
    (bind ?count 0)   
        
    (do-for-all-facts ((?p1 Student)) (subsetp ?data ?slotNamesFirstType)
            (printout t ?p1:Name crlf)
            (bind ?count 1)
    )

    (do-for-all-facts ((?p2 Programm)) (subsetp ?data ?slotNamesSecondType)
            (printout t ?p2:Name crlf)
            (bind ?count 1)
    )

    (if (= ?count 0)
    then
        (printout t "Такого фрейма нет!" crlf))
    (printout t " " crlf)
)


(deffunction print_menu ()
    (retract 0)
    (bind ?choice 1)
    (while (and (< ?choice 9)(> ?choice 0)) do
        (printout t "1 - Добавить фрейм" crlf)
        (printout t "2 - Изменить фрейм" crlf)
        (printout t "3 - Удалить фрейм" crlf)  
        (printout t "4 - Вывести список фреймов" crlf)
        (printout t "5 - Cемантический поиск('Ничего Лишнего')" crlf)
        (printout t "6 - Cемантический поиск('Все Подходящее')" crlf)
        (printout t "7 - Синтаксический поиск('Ничего Лишнего')" crlf)
        (printout t "8 - Синтаксический поиск('Все Подходящее')" crlf)
        (printout t "9 - Выход" crlf)
        (bind ?choice (ask-question "Выберите пункт: "1 2 3 4 5 6 7 8 9))
        (switch ?choice  
            (case 1 then
                (add_frame)
            )
            (case 2 then
                (edit_frame)
            )
            (case 3 then
                (delete_frame)
            )
            (case 4 then
                (print_frames)
            )
            (case 5 then
                (search_1_frame)
            )
            (case 6 then
                (search_2_frame)
            )
            (case 7 then
                (search_3_frame)
            )
            (case 8 then
                (search_4_frame)
            )
            (case 9 then
                (printout t "Выход" crlf)
            )
        )   
    )
)

(defrule start
   (initial-fact)
   =>
   (assert (Student (Name "stud1") (Name_S "Daniil") (Surname "Bolshakov") (SurSurname "Kirillovich") (Group 974) (Course 4)))
   (assert (Student (Name "stud2") (Name_S "Vasilyi") (Surname "Kochetov") (SurSurname "Gennadievich") (Group 974) (Course 3)))
   (assert (Student (Name "stud3") (Name_S "Ilya") (Surname "Trubnikov") (SurSurname "Alekseevich") (Group 973) (Course 2)))
   (assert (Student (Name "stud4") (Name_S "Alexei") (Surname "Ostrov") (SurSurname "Andreevich") (Group 972) (Course 1)))

   (assert (Programm (Name "prog1") (Name_L "C++") (Standart "C++11") (Size 3.5) (Long 300)))
   (assert (Programm (Name "prog2") (Name_L "C++") (Standart "C++11") (Size 4.) (Long 350)))
   (assert (Programm (Name "prog3") (Name_L "C++") (Standart "C++17") (Size 5.) (Long 400)))
   (assert (Programm (Name "prog4") (Name_L "C++") (Standart "C++17") (Size 6.5) (Long 400)))
   (print_menu)
)   
