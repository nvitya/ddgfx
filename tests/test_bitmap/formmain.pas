unit formMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ddgfx;


type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnExit : TButton;
    oglpanel : TPanel;
    pnl : TPanel;
    drawtimer : TTimer;
    procedure btnExitClick(Sender : TObject);
    procedure drawtimerTimer(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
  private

  public
    scene : TddScene;

    bmp : TPixmap;

    t : single; // time

    origw, origh : integer;

    procedure OglboxResize(Sender: TObject);

  end;

var
  frmMain : TfrmMain;

implementation

uses dglOpenGL;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender : TObject);
var
  pd, pend : PColorUint;
  col : TColorUint;
begin

  origw := oglpanel.Width;
  origh := oglpanel.Height;

  scene := TddScene.Create(self, oglpanel);

  scene.bgcolor.r := 0.4;
  scene.bgcolor.g := 0.4;
  scene.bgcolor.b := 0.2;

  bmp := scene.root.NewPixmap(200, 100);
  bmp.x := 100;
  bmp.y := 50;
  bmp.Clear($FF008000);

  col := $FF000000;

  pd := bmp.data;
  pend := pd + bmp.width * bmp.height;
  while pd < pend do
  begin
    pd^ := col;
    inc(col);
    inc(pd);
  end;
  bmp.needsupdate := true;
  bmp.scalex := 2;
  bmp.scaley := 2;
  bmp.alpha := 0.5;
  bmp.rotation := -25;

  scene.OnResize := @OglboxResize;

  drawtimer.Enabled := true;
  t := 0;
end;

procedure TfrmMain.btnExitClick(Sender : TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.drawtimerTimer(Sender : TObject);
begin

  // modify the shapes
  t += 1;

  bmp.alpha := 1 + 0.5* sin(t / 50);
  bmp.scalex := 1 + 0.5 * sin(t / 100);
  bmp.rotation := 20 + 40 * sin(t / 70);

  // for proper rescaling effect the x,y should remain zero
  scene.root.x := 100 + 50*sin(t / 20);
  scene.root.y :=  50 + 50*cos(t / 20);

  scene.root.scalex := oglpanel.Width / origw;
  scene.root.scaley := oglpanel.Height / origh;

  scene.Repaint;
end;

procedure TfrmMain.FormDestroy(Sender : TObject);
begin
end;

procedure TfrmMain.OglboxResize(Sender : TObject);
begin

end;


end.

