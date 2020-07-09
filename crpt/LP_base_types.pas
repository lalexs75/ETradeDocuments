{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020 Lagunov Aleksey alexs75@yandex.ru

  base on docs from http://api-docs.diadoc.ru

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

unit LP_base_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractSerializationObjects;

type
  //IPv4. Запись в виде четырёх десятичных чисел (от 0 до 255), разделённых точками.
  TIPv4Address_type = String;
  //IPv6. Запись в виде восьми четырёхзначных шестнадцатеричных чисел (групп по четыре символа), разделённых двоеточием.
  TIPv6Address_type = String;
  //Текстовое значение не более 200 символов
  Tstring200_type = String;
  //Текстовое значение не более 255 символов
  Tstring255_type = String;
  //Произвольная строка длинной до 1000 символов
  Tstring1000_type = String;
  //ИНН Участника оборота
  TTRADE_PARTICIPANT_INN_type = String;
  //ИНН ЦЭМ
  TLABELLING_CENTER_INN_type = String;
  //Дата в формате ДД.ММ.ГГГГ
  Tdate_type = String;
  Tdatetimeoffset = TDateTime;
  //Номер документа, но не более 200 символов
  Tdocument_number_200_type = String;
  //Справочник видов оборота товара
  //1 - Продажа-покупка
  //2 - Комиссия
  //3 - Агент
  //4 - Подряд
  Tturnover_type_enum = Longint;
  //Уникальный идентификатор товара. Количество символов: 31 либо 37 (с разделителями), 38 либо 44 (без разделителей).
  Tgs1_uit_type = String;
  //Уникальный идентификатор транспортной упаковки
  Tgs1_uitu_type = String;
  //Стоимость/Налог
  Tprice_type = Double;
  //GUID
  Tguid_type = String;
  //Регистрационный токен СУЗ
  Ttoken_type = String;
  //Товарная группа ТН ВЭД ЕАС
  Ttnved_group_type = Int64;
  //Код ТН ВЭД ЕАС
  Ttnved_code_type = Int64;
  //Справочник видов вывода из оборота
  //1 - Розничная продажа
  //2 -	Экспорт в страны ЕАЭС
  //3 - Экспорт за пределы стран ЕАЭС
  //4 -	Кредитный договор
  //5 -	Порча или утеря товара
  //6 -	Безвозмездная передача
  //7 -	Возврат физическому лицу (при невозможности реализовать товар по договору комиссии товар возвращается физическому лицу 	 обратно)
  //8 -	Банкротство или ликвидация ЮЛ/ИП
  //9 -	Реализации конфискованных товаров
  //10 - Использование на предприятии
  //11 - Договор рассрочки
  //12 - Розничная продажа ККТ
  Taction_type = Longint;
  //Справочник видов документа
  //1 - Фискальный документ
  //2 -	Бланк строгой отчетности (БСО)
  //3 - Договор
  //4 -	Акт уничтожения
  //5 -	Товарная накладная
  //6 -	Счет-фактура
  //7 -	Прочее
  Tdoc_type_enum = Longint;
  //Уникальный идентификатор транспортной упаковки или товара
  Tgs1_uit_uitu_type = String;
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;

  {  Tfias_address_type  }
  //Адрес в формате ФИАС
  Tfias_address_type = class(TXmlSerializationObject)
  private
    Faoguid:Taoguid;
    Fhouseguid:Thouseguid;
    Fflat:Tflat;
    procedure Setaoguid( AValue:Taoguid);
    procedure Sethouseguid( AValue:Thouseguid);
    procedure Setflat( AValue:Tflat);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Глобальный уникальный идентификатор адресного объекта
    property aoguid:Taoguid read Faoguid write Setaoguid;
    //Глобальный уникальный идентификатор домаОбязателен при наличии
    property houseguid:Thouseguid read Fhouseguid write Sethouseguid;
    //КвартираОбязателен при наличии
    property flat:Tflat read Fflat write Setflat;
  end;

implementation

  {  Tfias_address_type  }
procedure Tfias_address_type.Setaoguid(AValue: Taoguid);
begin
  CheckStrMinSize('aoguid', AValue);
  CheckStrMaxSize('aoguid', AValue);
  Faoguid:=AValue;
  ModifiedProperty('aoguid');
end;

procedure Tfias_address_type.Sethouseguid(AValue: Thouseguid);
begin
  CheckStrMinSize('houseguid', AValue);
  CheckStrMaxSize('houseguid', AValue);
  Fhouseguid:=AValue;
  ModifiedProperty('houseguid');
end;

procedure Tfias_address_type.Setflat(AValue: Tflat);
begin
  CheckStrMinSize('flat', AValue);
  CheckStrMaxSize('flat', AValue);
  Fflat:=AValue;
  ModifiedProperty('flat');
end;

procedure Tfias_address_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('aoguid', 'aoguid', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('houseguid', 'houseguid', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('flat', 'flat', [xsaSimpleObject], '', 1, 20);
end;

procedure Tfias_address_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tfias_address_type.Destroy;
begin
  inherited Destroy;
end;

constructor Tfias_address_type.Create;
begin
  inherited Create;
end;

end.
