package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3.ddd3domain.model.User;
import com.luma.wadloader3.ddd3domain.repository.UserRepo;
import com.luma.wadloader3api.api.UserApi;
import com.luma.wadloader3api.model.NewUserDto;
import com.luma.wadloader3api.model.UserDto;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
public class UserController implements UserApi {

    private final UserRepo userRepo;

    @Override
    public ResponseEntity<UserDto> userNewPost(@Valid NewUserDto newUserDto) {
        if (userRepo.existsByUsername(newUserDto.getName())) {
            return ResponseEntity.badRequest().build();
        }
        //TODO hash password
        User user = userRepo.saveAndFlush(new User(newUserDto.getName(), newUserDto.getPassword()));
        return ResponseEntity.ok(new UserDto(user.getUsername()));
    }


}
