report zaamr012 no standard page heading line-size 80
                line-count 65 message-id zs.
*Program changes
*TR66 14/06/2006 Mohammad Khan  Add more fields for update.
*
tables: anla.

data: tmp_bukrs like anla-bukrs,
      tmp_anln1 like anla-anln1,
      tmp_anln2 like anla-anln2,
      tmp_aktiv like anla-aktiv,
      tmp_zugdt like anla-zugdt,
      tmp_zujhr like anla-zujhr,
      tmp_zuper like anla-zuper.

************************************************************************
* begin of selection screen
************************************************************************
selection-screen begin of block box1 with frame.
  parameters: cmpnycd like anla-bukrs obligatory.
  parameters: masset like anla-anln1 obligatory.
  parameters: sasset like anla-anln2 obligatory.
  parameters: avddate like anla-aktiv.
  parameters: paqdate like anla-zugdt.
*  parameters: paqdate like anla-zugdt obligatory.
  parameters: paqyear like anla-zujhr.
  parameters: paqper like anla-zuper.
  parameters: upddata as checkbox.
selection-screen end of block box1.
************************************************************************
* end of selection screen
************************************************************************

start-of-selection.
  perform display_asset_data.
end-of-selection.

form display_asset_data.
  write / sy-uline.
  write: /001 text-000,
          020 sy-datum.
  write: /001 text-001,
          020 cmpnycd.
  write: /001 text-002,
          020 masset.
  write: /001 text-003,
          020 sasset.

  write / sy-uline.
  select bukrs anln1 anln2 aktiv zugdt zujhr zuper
    into (tmp_bukrs,tmp_anln1,tmp_anln2,tmp_aktiv,tmp_zugdt,
          tmp_zujhr,tmp_zuper)
    from anla
    where bukrs = cmpnycd and anln1 = masset and anln2 = sasset.
  endselect.
  if sy-subrc <> 0.
    write: /001 text-007.
    write / sy-uline.
  else.
    write: /001 text-015,
            030 tmp_aktiv.
    write: /001 text-004,
            030 tmp_zugdt.
    write: /001 text-005,
            030 tmp_zujhr.
    write: /001 text-006,
            030 tmp_zuper.

    write / sy-uline.
    write: /001 text-014,      "new
            030 avddate.       "new
    write: /001 text-008,
            030 paqdate.
    write: /001 text-009,
            030 paqyear.
    write: /001 text-010,
            030 paqper.

    write / sy-uline.
    if upddata = 'X'.
      perform update_asset_data.
    else.
      write: /001 text-011.
      write / sy-uline.
    endif.
  endif.
endform.

form update_asset_data.
  update anla
    set zugdt = paqdate
        aktiv = avddate              "Inserted line
        zujhr = paqyear
        zuper = paqper
    where bukrs = cmpnycd and anln1 = masset and anln2 = sasset.
  if sy-subrc <> 0.
    write: /001 text-012,
            035 sy-subrc.
    write / sy-uline.
  else.
    write: /001 text-013.

    write / sy-uline.
    select bukrs anln1 anln2 aktiv zugdt zujhr zuper
      into (tmp_bukrs,tmp_anln1,tmp_anln2,tmp_aktiv,
            tmp_zugdt,tmp_zujhr,tmp_zuper)
      from anla
      where bukrs = cmpnycd and anln1 = masset and anln2 = sasset.
    endselect.
    if sy-subrc <> 0.
      write: /001 text-007.
      write / sy-uline.
    else.
      write: /001 text-015,
              030 tmp_aktiv.
      write: /001 text-004,
              030 tmp_zugdt.
      write: /001 text-005,
              030 tmp_zujhr.
      write: /001 text-006,
              030 tmp_zuper.
      write / sy-uline.
    endif.
  endif.
endform.
