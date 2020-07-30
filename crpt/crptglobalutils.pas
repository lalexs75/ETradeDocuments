{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020 Lagunov Aleksey alexs75@yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit CrptGlobalUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CrptGlobalTypes;

function CISStatusDecode(AStatus:string):TCISStatus;
function CISStatusEncode(AStatus:TCISStatus):string;

function DocStatusDecode(AStatus:string):TDocStatus;
function DocStatusEncode(AStatus:TDocStatus):string;
implementation

function CISStatusDecode(AStatus: string): TCISStatus;
begin
  AStatus:=UpperCase(AStatus);
  if AStatus = 'EMITTED' then Result:=EMITTED
  else
  if AStatus = 'APPLIED' then Result:=APPLIED
  else
  if AStatus = 'INTRODUCED' then Result:=INTRODUCED
  else
  if AStatus = 'WRITTEN_OFF' then Result:=WRITTEN_OFF
  else
  if AStatus = 'RETIRED' then Result:=RETIRED
  else
  if AStatus = 'RESERVED_NOT_USED' then Result:=RESERVED_NOT_USED
  else
  if AStatus = 'INTRODUCED_RETURNED' then Result:=INTRODUCED_RETURNED
  else
  if AStatus = 'DISAGGREGATED' then Result:=DISAGGREGATED
  else
  if AStatus = 'WAIT_SHIPMENT' then Result:=WAIT_SHIPMENT
  else
  if AStatus = 'EXPORTED' then Result:=EXPORTED
  else
  if AStatus = 'LOAN_RETIRED' then Result:=LOAN_RETIRED
  else
  if AStatus = 'REMARK_RETIRED' then Result:=REMARK_RETIRED
  else
  if AStatus = 'APPLIED_NOT_PAID' then Result:=APPLIED_NOT_PAID
  else
  if AStatus = 'FTS_RESPOND_NOT_OK' then Result:=FTS_RESPOND_NOT_OK
  else
  if AStatus = 'FTS_RESPOND_WAITING' then Result:=FTS_RESPOND_WAITING
  else
  if AStatus = 'FTS_CONTROL' then Result:=FTS_CONTROL
  else
    Result:=CISStatusError

end;

function CISStatusEncode(AStatus: TCISStatus): string;
begin
  case AStatus of
    CISStatusError: Result:='Ошибка';
    EMITTED: Result:='Эмитирован';
    APPLIED: Result:='Нанесён';
    INTRODUCED: Result:='Введен в оборот';
    WRITTEN_OFF: Result:='Списан';
    RETIRED: Result:='Выведен из оборота Withdrawn';
    RESERVED_NOT_USED: Result:='Зарезервировано. Не использовать';
    INTRODUCED_RETURNED: Result:='Возвращён в оборот';
    DISAGGREGATED: Result:='Дезагрегирован';
    WAIT_SHIPMENT: Result:='Ожидает подтверждения приемки';
    EXPORTED: Result:='Используется для документов экспорта';
    LOAN_RETIRED: Result:='Выведен из оборота по договору рассрочки';
    REMARK_RETIRED: Result:='Выведен из оборота при перемаркировке';
    APPLIED_NOT_PAID: Result:='Нанесён, не оплачен';
    FTS_RESPOND_NOT_OK: Result:='Отрицательное решение ФТС';
    FTS_RESPOND_WAITING: Result:='Ожидает подтверждение ФТС';
    FTS_CONTROL: Result:='На контроле ФТС';
  end;
end;

function DocStatusDecode(AStatus: string): TDocStatus;
begin
  AStatus:=UpperCase(AStatus);
  if AStatus = 'UNDEFINED' then
    Result:=UNDEFINED
  else
  if AStatus = 'IN_PROGRESS' then
    Result:=IN_PROGRESS
  else
  if AStatus = 'CHECKED_OK' then
    Result:=CHECKED_OK
  else
  if AStatus = 'CHECKED_NOT_OK' then
    Result:=CHECKED_NOT_OK
  else
  if AStatus = 'PROCESSING_ERROR' then
    Result:=PROCESSING_ERROR
  else
  if AStatus = 'CANCELLED' then
    Result:=CANCELLED
  else
  if AStatus = 'WAIT_ACCEPTANCE' then
    Result:=WAIT_ACCEPTANCE
  else
  if AStatus = 'WAIT_PARTICIPANT_REGISTRATION' then
    Result:=WAIT_PARTICIPANT_REGISTRATION
  else
  if AStatus = 'WAIT_FOR_CONTINUATION' then
    Result:=WAIT_FOR_CONTINUATION
  else
  if AStatus = 'ACCEPTED' then
    Result:=ACCEPTED
  else
    Result:=UNDEFINED;
end;

function DocStatusEncode(AStatus: TDocStatus): string;
begin
  case AStatus of
    UNDEFINED: Result:='Не определён';
    IN_PROGRESS: Result:='Проверяется';
    CHECKED_OK: Result:='Оформлен';
    CHECKED_NOT_OK: Result:='Ошибка при проверке';
    PROCESSING_ERROR: Result:='Ошибка при обработке';
    CANCELLED: Result:='Документ отменён';
    WAIT_ACCEPTANCE: Result:='Ожидание приемку';
    WAIT_PARTICIPANT_REGISTRATION: Result:='Ожидает регистрации участника в ГИС МТ';
    WAIT_FOR_CONTINUATION: Result:='Ожидает продолжения процессинга документа';
    ACCEPTED: Result:='Принят';
  end;
end;

end.

