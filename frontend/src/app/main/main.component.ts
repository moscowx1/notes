import { Component } from '@angular/core';
import { NavigatorService } from 'src/services/navigator.service';

@Component({
  selector: 'app-main',
  templateUrl: './main.component.html',
  styleUrls: ['./main.component.scss'],
})
export class MainComponent {
  constructor(public navigator: NavigatorService) {
  }
}
