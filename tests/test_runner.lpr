program test_runner;

{$mode objfpc}{$H+}

uses

  Interfaces, Forms, GuiTestRunner, test_template_core,
  test_template_parser,
  test_template_exec_builtins,
  test_template_exec_controlflow,
  test_template_exec_range,
  test_template_exec_templates,
  test_template_exec_missingkey,
  test_template_exec_rtti,
  test_template_examples_codegen;

  {$R *.res}

  begin
    Application.Initialize;
    Application.CreateForm(TGuiTestRunner, TestRunner);
    Application.Run;
  end.

