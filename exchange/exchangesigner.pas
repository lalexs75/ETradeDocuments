{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота

  Структуры данных базируются на основании "Приказ ФНС РФ от 19.12.2018 N ММВ-7-15/820@"

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

unit ExchangeSigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc;

type

  { TExchangeSigner }

  TExchangeSigner = class(TXmlSerializationObject)   //%Таблица 7.18
  private
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property SignerPowers:string;
    property Status:string;
    property SignerPowersBase:string;
    (*
    Основание полномочий (доверия) организации 	ОснПолнОрг 	А 	T(1-255) 	Н 	Обязателен для Статус=3. Указываются основания полномочий (доверия) организации

    Физическое лицо | 	ФЛ 	С 		О 	Типовой элемент <СвФЛТип>.
    Состав элемента представлен в таблице 7.21

    Индивидуальный предприниматель | 	ИП 	С 		О 	Типовой элемент <СвИПТип>.
    Состав элемента представлен в таблице 7.20

    Представитель юридического лица 	ЮЛ 	С 		О 	Состав элемента представлен в таблице 7.19
    *)
  end;

  { TExchangeSignerList }

  TExchangeSignerList = class(TXmlSerializationObjectList) //%Таблица 7.18
  private
    function GetItem(AIndex: Integer): TExchangeSigner; inline;
  public
    constructor Create;
    property Item[AIndex:Integer]:TExchangeSigner read GetItem;
  end;

implementation

{ TExchangeSigner }

procedure TExchangeSigner.InternalRegisterPropertys;
begin
  (*
  Область полномочий 	ОблПолн 	А 	T(=1) 	ОК 	Принимает значение:
  1 - лицо, совершившее сделку, операцию |
  2 - лицо, совершившее сделку, операцию и ответственное за ее оформление |
  3 - лицо, ответственное за оформление свершившегося события

  Статус 	Статус 	А 	T(=1) 	ОК 	Принимает значение:
  3 - работник иной уполномоченной организации |
  4 - уполномоченное физическое лицо, в том числе индивидуальный предприниматель |
  5 - работник организации - покупателя |
  6 - работник организации - составителя файла обмена информации покупателя, если составитель файла обмена информации покупателя не является покупателем

  Основание полномочий (доверия) 	ОснПолн 	А 	T(1-255) 	О 	Для Статус=1 или Статус=2 или Статус=3 указываются "Должностные обязанности" по умолчанию или иные основания полномочий (доверия).
  Для Статус=4 указываются основания полномочий (доверия)

  Основание полномочий (доверия) организации 	ОснПолнОрг 	А 	T(1-255) 	Н 	Обязателен для Статус=3. Указываются основания полномочий (доверия) организации

  Физическое лицо | 	ФЛ 	С 		О 	Типовой элемент <СвФЛТип>.
  Состав элемента представлен в таблице 7.21

  Индивидуальный предприниматель | 	ИП 	С 		О 	Типовой элемент <СвИПТип>.
  Состав элемента представлен в таблице 7.20

  Представитель юридического лица 	ЮЛ 	С 		О 	Состав элемента представлен в таблице 7.19
  *)
end;

procedure TExchangeSigner.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TExchangeSigner.Destroy;
begin
  inherited Destroy;
end;

{ TExchangeSignerList }

function TExchangeSignerList.GetItem(AIndex: Integer): TExchangeSigner;
begin
  Result:=TExchangeSigner(InternalGetItem(AIndex));
end;

constructor TExchangeSignerList.Create;
begin
  inherited Create(TExchangeSigner)
end;

end.

