{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020-2021 Lagunov Aleksey alexs75@yandex.ru

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

unit remark;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractSerializationObjects, LP_base_types_v2;

type

  {  Forward declarations  }
  Tremark = class;
  Tremark_element = class;
  Tremark_products_list = class;
  Tremark_products_list_product = class;

  {  Generic classes for collections  }
  TremarkList = specialize GXMLSerializationObjectList<Tremark>;
  Tremark_elementList = specialize GXMLSerializationObjectList<Tremark_element>;
  Tremark_products_listList = specialize GXMLSerializationObjectList<Tremark_products_list>;
  Tremark_products_list_productList = specialize GXMLSerializationObjectList<Tremark_products_list_product>;

  {  Tremark  }
  //Перемаркировка
  Tremark = class(TXmlSerializationObject)
  private
    Ftrade_participant_inn:Tinn_type;
    Fremark_date:Tdate_type;
    Fremark_cause:Tremark_cause_type;
    Fproducts_list:Tremark_products_list;
    Fversion:Longint;
    procedure Settrade_participant_inn( AValue:Tinn_type);
    procedure Setremark_date( AValue:Tdate_type);
    procedure Setremark_cause( AValue:Tremark_cause_type);
    procedure Setversion( AValue:Longint);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //ИНН участника оборота
    property trade_participant_inn:Tinn_type read Ftrade_participant_inn write Settrade_participant_inn;
    //Дата перемаркировки
    property remark_date:Tdate_type read Fremark_date write Setremark_date;
    //Испорчено либо утеряно СИ с КМ - KM_SPOILED
    //Выявлены ошибки описания товара - DESCRIPTION_ERRORS
    //Возврат от розничного покупателя - RETAIL_RETURN
    //Возврат в случае дистанционной продажи - REMOTE_SALE_RETURN
    //Возврат от конечного покупателя (юр.лица/ИП) - LEGAL_RETURN
    //Решение о реализации товаров, приобретенных ранее в целях, не связанных с их реализацией - INTERNAL_RETURN
    //Возврат ранее экспортированного в ЕАЭС - EEC_EXPORT_RETURN
    property remark_cause:Tremark_cause_type read Fremark_cause write Setremark_cause;
    //Список товаров
    property products_list:Tremark_products_list read Fproducts_list;
    property version:Longint read Fversion write Setversion;
  end;

  {  Tremark_element  }
  Tremark_element = class(Tremark)
  private
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  {  Tremark_products_list  }
  Tremark_products_list = class(TXmlSerializationObject)
  private
    Fproduct:Tremark_products_list_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Параметры товара
    property product:Tremark_products_list_productList read Fproduct;
  end;

  {  Tremark_products_list_product  }
  Tremark_products_list_product = class(TXmlSerializationObject)
  private
    Flast_ki:Tki_type;
    Fnew_ki:Tki_type;
    Ftnved_code_10:Ttnved_code_type;
    Fproduction_country:Tstring255_type;
    Fcolor:Tstring1024_type;
    Fproduct_size:Tstring1024_type;
    Fremark_date:Tdate_type;
    Fpaid:Boolean;
    Fprimary_document_type:String;
    Fprimary_document_custom_name:Tstring255_type;
    Fprimary_document_number:Tstring255_type;
    Fprimary_document_date:Tdate_type;
    Fcertificate_type:Tcertificate_type_type;
    Fcertificate_number:Tstring255_type;
    Fcertificate_date:Tdate_type;
    procedure Setlast_ki( AValue:Tki_type);
    procedure Setnew_ki( AValue:Tki_type);
    procedure Settnved_code_10( AValue:Ttnved_code_type);
    procedure Setproduction_country( AValue:Tstring255_type);
    procedure Setcolor( AValue:Tstring1024_type);
    procedure Setproduct_size( AValue:Tstring1024_type);
    procedure Setremark_date( AValue:Tdate_type);
    procedure Setpaid( AValue:Boolean);
    procedure Setprimary_document_type( AValue:String);
    procedure Setprimary_document_custom_name( AValue:Tstring255_type);
    procedure Setprimary_document_number( AValue:Tstring255_type);
    procedure Setprimary_document_date( AValue:Tdate_type);
    procedure Setcertificate_type( AValue:Tcertificate_type_type);
    procedure Setcertificate_number( AValue:Tstring255_type);
    procedure Setcertificate_date( AValue:Tdate_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Предыдущий код идентификации единицы товара (КИ)
    property last_ki:Tki_type read Flast_ki write Setlast_ki;
    //Новый код идентификации единицы товара (КИ)
    property new_ki:Tki_type read Fnew_ki write Setnew_ki;
    //Код ТН ВЭД ЕАЭС товара
    property tnved_code_10:Ttnved_code_type read Ftnved_code_10 write Settnved_code_10;
    //Код страны производства по ОКСМ
    property production_country:Tstring255_type read Fproduction_country write Setproduction_country;
    //Цвет. Параметр обязателен для ТГ "Обувь
    property color:Tstring1024_type read Fcolor write Setcolor;
    //Размер в штихмассовой системе. Параметр обязателен для ТГ "Обувь
    property product_size:Tstring1024_type read Fproduct_size write Setproduct_size;
    //Дата перемаркировки
    property remark_date:Tdate_type read Fremark_date write Setremark_date;
    //Товар оплачен
    property paid:Boolean read Fpaid write Setpaid;
    //Тип первичного документа
    property primary_document_type:String read Fprimary_document_type write Setprimary_document_type;
    //Наименование первичного документа
    property primary_document_custom_name:Tstring255_type read Fprimary_document_custom_name write Setprimary_document_custom_name;
    //Номер первичного документа
    property primary_document_number:Tstring255_type read Fprimary_document_number write Setprimary_document_number;
    //Дата первичного документа
    property primary_document_date:Tdate_type read Fprimary_document_date write Setprimary_document_date;
    //Вид документа, подтверждающего соответствие
    property certificate_type:Tcertificate_type_type read Fcertificate_type write Setcertificate_type;
    //Номер документа, подтверждающего соответствие
    property certificate_number:Tstring255_type read Fcertificate_number write Setcertificate_number;
    //Дата документа, подтверждающего соответствие
    property certificate_date:Tdate_type read Fcertificate_date write Setcertificate_date;
  end;

implementation

  {  Tremark  }
procedure Tremark.Settrade_participant_inn(AValue: Tinn_type);
begin
  CheckStrMinSize('trade_participant_inn', AValue);
  CheckStrMaxSize('trade_participant_inn', AValue);
  Ftrade_participant_inn:=AValue;
  ModifiedProperty('trade_participant_inn');
end;

procedure Tremark.Setremark_date(AValue: Tdate_type);
begin
  CheckStrMinSize('remark_date', AValue);
  CheckStrMaxSize('remark_date', AValue);
  Fremark_date:=AValue;
  ModifiedProperty('remark_date');
end;

procedure Tremark.Setremark_cause(AValue: Tremark_cause_type);
begin
  CheckLockupValue('remark_cause', AValue);
  Fremark_cause:=AValue;
  ModifiedProperty('remark_cause');
end;

procedure Tremark.Setversion(AValue: Longint);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure Tremark.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('trade_participant_inn', 'trade_participant_inn', [xsaSimpleObject], '', 9, 12);
  P:=RegisterProperty('remark_date', 'remark_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('remark_cause', 'remark_cause', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('KM_SPOILED');
    P.ValidList.Add('DESCRIPTION_ERRORS');
    P.ValidList.Add('RETAIL_RETURN');
    P.ValidList.Add('REMOTE_SALE_RETURN');
  P:=RegisterProperty('products_list', 'products_list', [], '', -1, -1);
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='6';
end;

procedure Tremark.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproducts_list:=Tremark_products_list.Create;
end;

destructor Tremark.Destroy;
begin
  Fproducts_list.Free;
  inherited Destroy;
end;

constructor Tremark.Create;
begin
  inherited Create;
  version:=6;
end;

  {  Tremark_element  }
procedure Tremark_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Tremark_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tremark_element.Destroy;
begin
  inherited Destroy;
end;

function Tremark_element.RootNodeName:string;
begin
  Result:='remark';
end;

constructor Tremark_element.Create;
begin
  inherited Create;
end;

  {  Tremark_products_list  }
procedure Tremark_products_list.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tremark_products_list.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tremark_products_list_productList.Create;
end;

destructor Tremark_products_list.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tremark_products_list.Create;
begin
  inherited Create;
end;

  {  Tremark_products_list_product  }
procedure Tremark_products_list_product.Setlast_ki(AValue: Tki_type);
begin
  CheckStrMinSize('last_ki', AValue);
  CheckStrMaxSize('last_ki', AValue);
  Flast_ki:=AValue;
  ModifiedProperty('last_ki');
end;

procedure Tremark_products_list_product.Setnew_ki(AValue: Tki_type);
begin
  CheckStrMinSize('new_ki', AValue);
  CheckStrMaxSize('new_ki', AValue);
  Fnew_ki:=AValue;
  ModifiedProperty('new_ki');
end;

procedure Tremark_products_list_product.Settnved_code_10(AValue: Ttnved_code_type);
begin
  Ftnved_code_10:=AValue;
  ModifiedProperty('tnved_code_10');
end;

procedure Tremark_products_list_product.Setproduction_country(AValue: Tstring255_type);
begin
  CheckStrMinSize('production_country', AValue);
  CheckStrMaxSize('production_country', AValue);
  Fproduction_country:=AValue;
  ModifiedProperty('production_country');
end;

procedure Tremark_products_list_product.Setcolor(AValue: Tstring1024_type);
begin
  CheckStrMinSize('color', AValue);
  CheckStrMaxSize('color', AValue);
  Fcolor:=AValue;
  ModifiedProperty('color');
end;

procedure Tremark_products_list_product.Setproduct_size(AValue: Tstring1024_type);
begin
  CheckStrMinSize('product_size', AValue);
  CheckStrMaxSize('product_size', AValue);
  Fproduct_size:=AValue;
  ModifiedProperty('product_size');
end;

procedure Tremark_products_list_product.Setremark_date(AValue: Tdate_type);
begin
  CheckStrMinSize('remark_date', AValue);
  CheckStrMaxSize('remark_date', AValue);
  Fremark_date:=AValue;
  ModifiedProperty('remark_date');
end;

procedure Tremark_products_list_product.Setpaid(AValue: Boolean);
begin
  Fpaid:=AValue;
  ModifiedProperty('paid');
end;

procedure Tremark_products_list_product.Setprimary_document_type(AValue: String);
begin
  Fprimary_document_type:=AValue;
  ModifiedProperty('primary_document_type');
end;

procedure Tremark_products_list_product.Setprimary_document_custom_name(AValue: Tstring255_type);
begin
  CheckStrMinSize('primary_document_custom_name', AValue);
  CheckStrMaxSize('primary_document_custom_name', AValue);
  Fprimary_document_custom_name:=AValue;
  ModifiedProperty('primary_document_custom_name');
end;

procedure Tremark_products_list_product.Setprimary_document_number(AValue: Tstring255_type);
begin
  CheckStrMinSize('primary_document_number', AValue);
  CheckStrMaxSize('primary_document_number', AValue);
  Fprimary_document_number:=AValue;
  ModifiedProperty('primary_document_number');
end;

procedure Tremark_products_list_product.Setprimary_document_date(AValue: Tdate_type);
begin
  CheckStrMinSize('primary_document_date', AValue);
  CheckStrMaxSize('primary_document_date', AValue);
  Fprimary_document_date:=AValue;
  ModifiedProperty('primary_document_date');
end;

procedure Tremark_products_list_product.Setcertificate_type(AValue: Tcertificate_type_type);
begin
  CheckLockupValue('certificate_type', AValue);
  Fcertificate_type:=AValue;
  ModifiedProperty('certificate_type');
end;

procedure Tremark_products_list_product.Setcertificate_number(AValue: Tstring255_type);
begin
  CheckStrMinSize('certificate_number', AValue);
  CheckStrMaxSize('certificate_number', AValue);
  Fcertificate_number:=AValue;
  ModifiedProperty('certificate_number');
end;

procedure Tremark_products_list_product.Setcertificate_date(AValue: Tdate_type);
begin
  CheckStrMinSize('certificate_date', AValue);
  CheckStrMaxSize('certificate_date', AValue);
  Fcertificate_date:=AValue;
  ModifiedProperty('certificate_date');
end;

procedure Tremark_products_list_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('last_ki', 'last_ki', [xsaSimpleObject], '', 25, 45);
  P:=RegisterProperty('new_ki', 'new_ki', [xsaSimpleObject], '', 25, 45);
  P:=RegisterProperty('tnved_code_10', 'tnved_code_10', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('production_country', 'production_country', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('color', 'color', [xsaSimpleObject], '', 1, 1024);
  P:=RegisterProperty('product_size', 'product_size', [xsaSimpleObject], '', 1, 1024);
  P:=RegisterProperty('remark_date', 'remark_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('paid', 'paid', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('primary_document_type', 'primary_document_type', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('primary_document_custom_name', 'primary_document_custom_name', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('primary_document_number', 'primary_document_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('primary_document_date', 'primary_document_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('certificate_type', 'certificate_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('CONFORMITY_CERTIFICATE');
    P.ValidList.Add('CONFORMITY_DECLARATION');
  P:=RegisterProperty('certificate_number', 'certificate_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('certificate_date', 'certificate_date', [xsaSimpleObject], '', 10, 10);
end;

procedure Tremark_products_list_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tremark_products_list_product.Destroy;
begin
  inherited Destroy;
end;

constructor Tremark_products_list_product.Create;
begin
  inherited Create;
end;

end.
