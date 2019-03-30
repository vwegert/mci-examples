"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcx_mcibuch_dynamic_check DEFINITION PUBLIC FINAL CREATE PUBLIC
  INHERITING FROM cx_dynamic_check.

  PUBLIC SECTION.

    INTERFACES if_t100_message.

    CONSTANTS:
      BEGIN OF zcx_mcibuch_dynamic_check,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_mcibuch_dynamic_check.
    CONSTANTS:
      BEGIN OF unsupported_type,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unsupported_type.
    CONSTANTS:
      BEGIN OF property_error,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE 'PROPERTY_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF property_error.
    DATA property_name TYPE n1mci_property_name.

    METHODS constructor
      IMPORTING
        !textid        LIKE if_t100_message=>t100key OPTIONAL
        !previous      LIKE previous OPTIONAL
        !property_name TYPE n1mci_property_name OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_mcibuch_dynamic_check IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->property_name = property_name.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_mcibuch_dynamic_check.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
