import 'package:flutter/material.dart';
import 'package:front/api/api.dart';
import 'package:front/api/auth_data.dart';
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
  late final GlobalKey<FormState> _formKey;
  late final AuthData _data;
  bool _submitted = false;
  var _validateMode = AutovalidateMode.disabled;

  @override
  void initState() {
    super.initState();
    _formKey = GlobalKey<FormState>();
    _data = AuthData();
  }

  @override
  Widget build(BuildContext context) {
    return Form(
      key: _formKey,
      autovalidateMode: _validateMode,
      child: Container(
        constraints: BoxConstraints.loose(const Size(500, 500)),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.center,
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            TextFormField(
              decoration: const InputDecoration(labelText: 'login'),
              validator: Validators.composeWithSubmited(_submitted, [
                Validators.lengthAround(5, 15),
              ]),
              initialValue: _data.login,
              onChanged: (value) => _data.login = value,
            ),
            TextFormField(
              decoration: const InputDecoration(labelText: 'password'),
              autocorrect: false,
              obscureText: true,
              initialValue: _data.password,
              onChanged: (value) => _data.password = value,
              validator: Validators.composeWithSubmited(
                _submitted,
                [Validators.lengthAround(5, 20)],
              ),
            ),
            MaterialButton(
              child: const Text('Submit'),
              onPressed: () {
                setState(() {
                  _validateMode = AutovalidateMode.onUserInteraction;
                  _formKey.currentState?.validate();
                });

                var m = _formKey.currentState?.validate();
                if (m ?? false) {
                  _formKey.currentState!.save();
                  print(m);
                  print('ok');
                }
              },
            ),
          ],
        ),
      ),
    );
  }
  // return Center(
  //   child: Container(
  //     constraints: BoxConstraints.loose(const Size(500, 500)),
  //     child: Form(
  //       key: _formKey,
  //       autovalidateMode: AutovalidateMode.always,
  //       child: Column(
  //         crossAxisAlignment: CrossAxisAlignment.center,
  //         mainAxisAlignment: MainAxisAlignment.center,
  //         children: [
  //           TextFormField(
  //             decoration: const InputDecoration(labelText: 'login'),
  //             validator: Validators.composeWithSubmited(_submitted, [
  //               Validators.lengthAround(5, 15),
  //             ]),
  //             initialValue: _data.login,
  //             onChanged: (value) => _data.login = value,
  //           ),
  //           TextFormField(
  //             decoration: const InputDecoration(labelText: 'password'),
  //             autocorrect: false,
  //             obscureText: true,
  //             initialValue: _data.password,
  //             onChanged: (value) => _data.password = value,
  //             validator: Validators.composeWithSubmited(
  //               _submitted,
  //               [Validators.lengthAround(5, 20)],
  //             ),
  //           ),
  //           MaterialButton(
  //             child: const Text('Submit'),
  //             onPressed: () {
  //               setState(() {
  //                 _submitted = true;
  //               });

  //               var m = _formKey.currentState?.validate();
  //               if (m ?? false) {
  //                 _formKey.currentState!.save();
  //                 print(m);
  //                 print('ok');
  //               }
  //             },
  //           ),
  //         ],
  //       ),
  //     ),
  //   ),
  // );
}
