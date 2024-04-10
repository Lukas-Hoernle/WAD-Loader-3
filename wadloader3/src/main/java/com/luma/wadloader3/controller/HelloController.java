package com.luma.wadloader3.controller;

import com.luma.wadloader3api.api.HelloApi;
import com.luma.wadloader3api.model.HelloDto;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@CrossOrigin(origins = "http://localhost:5173")
public class HelloController implements HelloApi {

    @Override
    public ResponseEntity<HelloDto> helloNameGet(String name) {

        System.out.println(name);

        List<String> answers = List.of("Hallo", "Moin", "Servus");
        String response = answers.get((int) Math.round(Math.random() * 3));
        return ResponseEntity.ok(HelloDto.builder().name(name).greeting(response).build());
    }
}