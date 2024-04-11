package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3api.api.ApiApi;
import com.luma.wadloader3api.model.HelloDto;
import com.luma.wadloader3api.model.NewUserDto;
import com.luma.wadloader3api.model.UserDto;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class GreetingController implements ApiApi {

    @Override
    public ResponseEntity<HelloDto> apiHelloNameGet(String name) {
        return ResponseEntity.ok().body(HelloDto.builder().name(name).greeting("MOIN").build());
    }

    @Override
    public ResponseEntity<UserDto> apiUserNewPost(@Valid NewUserDto newUserDto) {
        return null;
    }
}
