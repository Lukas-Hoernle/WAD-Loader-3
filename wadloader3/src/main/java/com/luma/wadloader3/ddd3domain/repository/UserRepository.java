package com.luma.wadloader3.ddd3domain.repository;

import com.luma.wadloader3.ddd3domain.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface UserRepository extends JpaRepository<User, String> {
}
