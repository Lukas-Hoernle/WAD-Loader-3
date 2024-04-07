package com.luma.wadloader3.ddd3domain.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.*;


@Entity
@AllArgsConstructor
@Builder
@Table(name = "Users")
@NoArgsConstructor
@Getter
@Setter
public class User {
    @Id
    @Column(name = "username")
    private String username;

    @Column(name = "password")
    private String password;
}
