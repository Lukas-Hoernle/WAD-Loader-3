package com.luma.wadloader3.ddd0plugins.repository;

import com.luma.wadloader3.ddd3domain.model.User;
import com.luma.wadloader3.ddd3domain.repository.UserRepo;
import org.springframework.data.jpa.repository.JpaRepository;

import org.springframework.stereotype.Repository;

@Repository
public interface UserRepository extends UserRepo, JpaRepository<User, String> {
}
