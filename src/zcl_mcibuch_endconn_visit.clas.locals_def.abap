*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES: BEGIN OF t_movement_data,
         bapi2097ovout TYPE bapi2097ovout,
         bapi2097atout TYPE bapi2097atout,
       END OF t_movement_data.

CLASS lcl_bapi_wrapper DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcibuch_visit_bapi_wrapper .

    METHODS constructor
      IMPORTING
        i_destination TYPE rfcdest OPTIONAL.

  PRIVATE SECTION.
    DATA destination TYPE rfcdest.

ENDCLASS.
