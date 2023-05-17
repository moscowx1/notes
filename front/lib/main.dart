import 'package:flutter/material.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:form_builder_validators/form_builder_validators.dart';
import 'package:front/api/api.dart';
import 'package:front/api/login_req.dart';
import 'package:front/utils/validators.dart';
import 'globals.dart' as globals;

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    const appTitle = 'Form Validation Demo';
    return MaterialApp(
      title: appTitle,
      theme: ThemeData(
          primarySwatch: Colors.blue,
          visualDensity: VisualDensity.adaptivePlatformDensity),
      home: Scaffold(
        appBar: AppBar(
          title: const Text(appTitle),
        ),
        body: const AuthForm(),
      ),
    );
  }
}

class AuthForm extends StatefulWidget {
  const AuthForm({super.key});

  @override
  AuthFormState createState() {
    return AuthFormState();
  }
}

class AuthFormState extends State<AuthForm> {
  late final GlobalKey<FormBuilderState> _formKey;
  static const login = 'login';
  static const password = 'password';
  bool submitted = false;

  @override
  void initState() {
    super.initState();
    _formKey = GlobalKey<FormBuilderState>();
  }

  @override
  Widget build(BuildContext context) {
    return Center(
        child: Container(
      constraints: BoxConstraints.loose(const Size(500, 500)),
      child: FormBuilder(
        key: _formKey,
        autovalidateMode: AutovalidateMode.onUserInteraction,
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.center,
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
             MaterialButton(
                child: const Text('Submit'),
                onPressed: () {
                  setState(() {
                    submitted = true;
                  });

                  var m = _formKey.currentState?.validate();
                  if (_formKey.currentState?.validate() ?? false) {
                    print(m);
                    print('ok');
                  }
                }),
            FormBuilderTextField(
                name: login,
                maxLength: 15,
                decoration: const InputDecoration(labelText: 'login'),
                validator: Validators.composeWithSubmited(submitted, [
                  FormBuilderValidators.minLength(5),
                  Validators.dontStartEndWith(' '),
                ])),
            FormBuilderTextField(
              name: password,
              maxLength: 20,
              autocorrect: false,
              obscureText: true,
              decoration: const InputDecoration(labelText: 'password'),
              validator: Validators.composeWithSubmited(
                  submitted, [FormBuilderValidators.minLength(5)]),
            ),
            MaterialButton(
                child: const Text('Submit'),
                onPressed: () {
                  setState(() {
                    submitted = true;
                   });

                  var m = _formKey.currentState?.validate();
                  if (_formKey.currentState?.validate() ?? false) {
                    print(m);
                    print('ok');
                  }
                })
          ],
        ),
      ),
    ));
  }
}
